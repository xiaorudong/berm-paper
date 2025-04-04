# -*- coding: utf-8 -*-

import os
import pyreadr
import pandas as pd
import numpy as np
import time
from hi_lasso.hi_lasso import HiLasso  # Importing at the top

os.chdir("/Volumes/bachergroup/HDDataSelection_Method/")

errorsd_choice = [1, 3, 5]
s_vec = ["s1", "s2", "s3", "s4", "s5"]
timecost = pd.DataFrame(np.nan, index=range(1, 21), columns=[f"sd{sd}{s}" for sd in errorsd_choice for s in s_vec])

for t in range(1, 21):
    np.random.seed(t)

    for sd in errorsd_choice:
        for s in s_vec:
            file_path = f"RDATA/runif_hip+/60vars300obs_sd{sd}/sim_hip_60vars300obs_{t}_sd{sd}_{s}.RData"
            if os.path.exists(file_path):
                result = pyreadr.read_r(file_path)
                X_mat = result["X_mat"]
                y = result["y"]
                beta = result["beta"]

                t1 = time.time()
                model = HiLasso(q1=30, q2=30, L=30, alpha=0.05, logistic=False, random_state=None, parallel=False, n_jobs=None)
                model.fit(X_mat, y)
                t2 = time.time()

                difft = (t2 - t1) / 60  # convert seconds to minutes
                tname = f"sd{sd}{s}"
                timecost.loc[t, tname] = difft

                results_df = pd.DataFrame({
                    'est': model.coef_,
                    'truecoef': beta.squeeze().to_numpy(),
                    'VAR': [f"X{i + 1}" for i in range(len(beta))]
                })
                results_df['FROM'] = "HiLasso"
                results_df = results_df[['truecoef', 'est', 'FROM', 'VAR']]

                output_path = f"OUT/HiLasso/onlyres_HiLasso_hip_60vars300obs_{t}_sd{sd}_{s}.csv"
                os.makedirs(os.path.dirname(output_path), exist_ok=True)
                results_df.to_csv(output_path, index=False)

output_path = "OUT/HiLasso/timecost_HiLasso_hip_60vars300obs.csv"
os.makedirs(os.path.dirname(output_path), exist_ok=True)
timecost.to_csv(output_path, index=False)




timecost = pd.DataFrame(np.nan, index=range(1, 21), columns=[f"sd{sd}{s}" for sd in errorsd_choice for s in s_vec])

for t in range(1, 21):
    np.random.seed(t)

    for sd in errorsd_choice:
        for s in s_vec:
            file_path = f"RDATA/runif_hip+/500vars300obs_sd{sd}/sim_hip_500vars300obs_{t}_sd{sd}_{s}.RData"
            if os.path.exists(file_path):
                result = pyreadr.read_r(file_path)
                X_mat = result["X_mat"]
                y = result["y"]
                beta = result["beta"]

                t1 = time.time()
                model = HiLasso(q1=300, q2=300, L=30, alpha=0.05, logistic=False, random_state=None, parallel=False, n_jobs=None)
                model.fit(X_mat, y)
                t2 = time.time()

                difft = (t2 - t1) / 60  # convert seconds to minutes
                tname = f"sd{sd}{s}"
                timecost.loc[t, tname] = difft

                results_df = pd.DataFrame({
                    'est': model.coef_,
                    'truecoef': beta.squeeze().to_numpy(),
                    'VAR': [f"X{i + 1}" for i in range(len(beta))]
                })
                results_df['FROM'] = "HiLasso"
                results_df = results_df[['truecoef', 'est', 'FROM', 'VAR']]

                output_path = f"OUT/HiLasso/onlyres_HiLasso_hip_500vars300obs_{t}_sd{sd}_{s}.csv"
                os.makedirs(os.path.dirname(output_path), exist_ok=True)
                results_df.to_csv(output_path, index=False)

output_path = "OUT/HiLasso/timecost_HiLasso_hip_500vars300obs.csv"
os.makedirs(os.path.dirname(output_path), exist_ok=True)
timecost.to_csv(output_path, index=False)

