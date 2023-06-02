"""Dynamic and task-related higher-order interactions."""
import numpy as np
import xarray as xr
import pandas as pd
from numpy import genfromtxt

import itertools

from frites.conn import conn_io
from frites.io import logger, check_attrs
from frites.utils import parallel_func
from frites.core import copnorm_nd



def ent_g(x):
    """Entropy of a tensor of shape (..., n_vars, n_trials)"""
    nvarx, ntrl = x.shape[-2], x.shape[-1]

    # covariance
    c = np.einsum('...ij, ...kj->...ik', x, x)
    c /= float(ntrl - 1.)
    chc = np.linalg.cholesky(c)

    # entropy in nats
    hx = np.log(np.einsum('...ii->...i', chc)).sum(-1) + 0.5 * nvarx * (
        np.log(2 * np.pi) + 1.0)
    return hx


def compute_oinfo(x, ind):
    """Compute the O-info.

    x.shape = (..., n_vars, n_trials)
    ind = indices for tensor-computations
    """
    nvars = x.shape[-2]
    o = (nvars - 2) * ent_g(x)
    o += (ent_g(x[..., np.newaxis, :]) - ent_g(x[..., ind, :])).sum(1)
    return o


def combinations(n, k, roi, task_related=False):
    """Get combinations."""
    combs = np.array(list(itertools.combinations(np.arange(n), k)))

    # add behavior as a final columns
    if task_related:
        combs = np.c_[combs, np.full((combs.shape[0],), n)]

    # build brain region names
    roi_st = ['-'.join(r) for r in roi[combs].tolist()]

    return combs, roi_st


def conn_hoi(data, y=None, times=None, roi=None, minsize=3, maxsize=5,
             verbose=None):
    """Dynamic, possibly task-related, higher-order interactions.

    Parameters
    ----------
    data : array_like
        Electrophysiological data. Several input types are supported :

            * Standard NumPy arrays of shape (n_epochs, n_roi, n_times)
            * mne.Epochs
            * xarray.DataArray of shape (n_epochs, n_roi, n_times)

    y : array_like
        The feature of shape (n_trials,) for estimating task-related O-info.
    roi : array_like | None
        Array of region of interest name of shape (n_roi,)
    times : array_like | None
        Array of time points of shape (n_times,)
    minsize, maxsize : int | 3, 5
        Minimum and maximum size of the multiplets

    Returns
    -------
    oinfo : array_like
        The O-info array of shape (n_multiplets, n_times) where positive values
        reflect redundant dominated interactions and negative values stand for
        synergistic dominated interactions.
    """
    
    # ________________________________ INPUTS _________________________________
    # inputs conversion
    is_task_related = isinstance(y, (str, list, np.ndarray, tuple))
    kw_links = {'directed': False, 'net': False}
    data, cfg = conn_io(
        data, y=y, times=times, roi=roi, name='DynOinfo', verbose=verbose,
        kw_links=kw_links
    )

    # extract variables
    x, attrs = data.data, cfg['attrs']
    y, roi, times = data['y'].data, data['roi'].data, data['times'].data
    n_roi = len(roi)

    # get the maximum size of the multiplets investigated
    if not isinstance(maxsize, int):
        maxsize = n_roi
    maxsize = max(1, maxsize)
    assert maxsize > minsize

    logger.info(f"Compute the {'task-related ' * is_task_related} HOI "
                f"(min={minsize}; max={maxsize})")

    # ________________________________ O-INFO _________________________________
    logger.info("    Copnorm the data")

    # for task-related, add behavior along spatial dimension
    if is_task_related:
        y = np.tile(y.reshape(-1, 1, 1), (1, 1, len(times)))
        x = np.concatenate((x, y), axis=1)
        roi = np.r_[roi, ['beh']]

    # copnorm and demean the data
    x = copnorm_nd(x.copy(), axis=0)
    x = (x - x.mean(axis=0, keepdims=True))

    # make the data (n_times, n_roi, n_trials)
    x = x.transpose(2, 1, 0)

    oinfo, roi_o = [], []
    for msize in range(minsize, maxsize + 1):
        # ------------------------------ INDICES ------------------------------
        ish = msize if not is_task_related else msize + 1
        ind = np.zeros((ish, ish), dtype=int)
        vec = np.arange(ish)
        for shift in range(ish):
            ind[shift, :] = np.roll(vec, -shift)
        ind = ind[:, 1:]

        # ----------------------------- MULTIPLETS ----------------------------
        logger.info(f"    Multiplets of size {msize}")
        combs, _roi_o = combinations(
            n_roi, msize, roi, task_related=is_task_related)
        roi_o += _roi_o

        # ------------------------------- O-INFO ------------------------------
        for mult in combs:
            _oinfo = compute_oinfo(x[:, mult, :], ind)
            oinfo += [_oinfo]
    oinfo = np.stack(oinfo, 0)

    # _______________________________ OUTPUTS _________________________________
    attrs.update(dict(
        task_related=is_task_related, minsize=minsize, maxsize=maxsize
    ))
    oinfo = xr.DataArray(
        oinfo, dims=('roi', 'times'), coords=(roi_o, times), name="Oinfo",
        attrs=check_attrs(attrs)
    )

    return oinfo



if __name__ == '__main__':
    import matplotlib.pyplot as plt
    from frites import set_mpl_style
    import seaborn as sns
    set_mpl_style()

    ###########################################################################
    n_trials = 300
    n_roi = 6
    n_times = 600

    redundancy = [
        # (0, 1, 3),
        (2, 3, 5)
        # (1, 2, 4, 5)
    ]
    synergy = [
        (0, 1, 2)
    ]
    ###########################################################################

    def set_redundancy(x, redundancy, sl, win, trials):
        for m in redundancy:
            x[:, m, sl] += .6 * trials.reshape(-1, 1, 1) * win
        return x

    def set_synergy(x, synergy, sl, win, trials):
        for m in synergy:
            blocks = np.array_split(np.arange(n_trials), len(m))
            for n_b, b in enumerate(blocks):
                x[b, m[n_b], sl] += trials[b].reshape(-1, 1) * win[0, ...]
        return x


    # generate the data
    x = np.random.rand(n_trials, n_roi, n_times)
    roi = np.array([f"r{r}" for r in range(n_roi)])
    trials = np.random.rand(n_trials)
    # trials = np.arange(n_trials)
    # times = (np.arange(n_times) - 200) / 128.
    times = np.arange(n_times)
    win = np.hanning(100).reshape(1, 1, -1)

    # introduce (redundant, synergistic) information
    x = set_redundancy(x, redundancy, slice(200, 300), win, trials)
    x = set_synergy(x, synergy, slice(300, 400), win, trials)



    x = xr.DataArray(x, dims=('trials', 'roi', 'times'),
                     coords=(trials, roi, times))
    
    datas=np.array(pd.read_csv(r'C:\Users\lisa\Documents\Cours_Formations_Conf_Labmeeting\BrainHack\BrainHAck_2022_Marseille\HOI\Datas2.csv'))
    #datas=genfromtxt(r'C:\Users\lisa\Documents\Cours_Formations_Conf_Labmeeting\BrainHack\BrainHAck_2022_Marseille\HOI\Datas2.csv', delimiter=',')
    #datas=np.genfromtxt(r'C:\Users\lisa\Documents\Cours_Formations_Conf_Labmeeting\BrainHack\BrainHAck_2022_Marseille\HOI\Datas2.csv',delimiter=',',names=True)
    datas=pd.read_csv(r'C:\Users\lisa\Documents\Cours_Formations_Conf_Labmeeting\BrainHack\BrainHAck_2022_Marseille\HOI\Datas2.csv')
    datas = datas.iloc[:,[1,5,6,7,13,14,15]]
    arr=datas.values
    x = xr.DataArray(data=arr[...,np.newaxis], dims=["Suj", "Quest","Time"],
                     coords=(datas.index, datas.columns, [0]))


    oinfo = conn_hoi(x, minsize=3, maxsize=7, y=None, roi="Quest",
                     times="Time")
    
    # print(oinfo)
    vmin, vmax = np.nanpercentile(oinfo.data, [1, 99])
    minmax = max(abs(vmin), abs(vmax))
    vmin, vmax = -minmax, minmax

    # plot the results
    df = oinfo.to_pandas()
    
    plt.pcolormesh(
        df.columns, df.index, df.values, cmap='RdBu_r', vmin=vmin, vmax=vmax
    )
    plt.colorbar()
    plt.xlabel('Suj')
    plt.axvline(0., color='k')

    redu = ['-'.join(roi[list(r)]) for r in redundancy]
    syn = ['-'.join(roi[list(r)]) for r in synergy]
    for n_k, k in enumerate(oinfo['roi'].data):
        if k.replace('-beh', '') in redu:
            plt.gca().get_yticklabels()[n_k].set_color('red')
        if k.replace('-beh', '') in syn:
            plt.gca().get_yticklabels()[n_k].set_color('blue')

    plt.show()
