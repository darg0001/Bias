# 4/29/2018
from __future__ import division
import calendar
from IPython.display import Image
import matplotlib.pyplot as plt
from math import sqrt
import numpy as np
import pandas as pd
import os
import random
import re
import scipy
import sklearn
from sklearn import linear_model
from sklearn import metrics
from sklearn.ensemble import RandomForestRegressor
from sklearn.metrics import mean_squared_error
from sklearn.model_selection import KFold
from sklearn.model_selection import RandomizedSearchCV
from sklearn.model_selection import GridSearchCV
from sklearn.pipeline import Pipeline
from sklearn.externals.six import StringIO
from sklearn.tree import export_graphviz
import sys
import time
import xlsxwriter
