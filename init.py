#
# This file is automatically evaluated whenever inferior Python interpreter is spawned.
# See ./init/_py.el for more.
#
import pandas as pd
from pprint import pprint as pp
from importlib import reload

def to_numeric(data, columns = None):
    """Convert DataFrame values to numeric type.

It tries to remove any prefix or suffix of the string such as '$12,345' into '12345'.
# Convert all string-typed columns into numeric columns
>>> to_numeric(df)

# convert value and price columns only
>>> to_numeric(df, columns = ['value', 'price'])
"""
    if columns is None:
        columns = data.select_dtypes(include=['string']).columns
    data = data.copy()
    for c in columns:
        # the last component of the regex is supposed to remove any
        # suffix character like '%'.  However, it removes string 'NaN'
        # to ''.  It is fine as pd.to_numeric() will convert to NaN
        # anyway.
        cc = data[c].str.replace('(^\\$|,|[^0-9.,]+$)', '', regex=True)
        data[c] = pd.to_numeric(cc)
    return data


def describe_columns(df, outliers=0.0, columns=None):
    """Call describe() on columns after removing outliers

# call describe() for values between 5% and 95% percentile.
>>> describe_columns(df, outliers=0.05)
"""
    if columns is None:
        columns = df.columns

    rs = []
    for c in df.columns:
        if pd.api.types.is_number(df[c]):
            d = df.loc[df[c].between(df[c].quantile(outliers),
                                     df[c].quantile(1.0 - outliers)), [c]]
            rs.append(d.describe())
        else:
            rs.append(df[c].describe())
    return pd.concat(rs, axis=1)

def remove_outliers(df, outliers=0.0):
    fs = []
    for cn in df.columns:
        f = df.loc[df[cn].between(df[cn].quantile(outliers),
                                  df[cn].quantile(1.0 - outliers)), [cn]]
        fs.append(f)
    return pd.concat(fs, axis=1)
        
def plot_columns(df, outliers=0.0, title=None, ylabel=None):
    import seaborn as sns
    
    sns.violinplot(data=df, inner=None, color="lightgray")

    for i, c in enumerate(df.columns):
        dc = df.loc[df[c].between(df[c].quantile(outliers),
                                  df[c].quantile(1.0 - outliers)), [c]]
        cmin = dc.min()
        cmax = dc.max()
        cavg = dc.mean()
        cdev = dc.std()
        # x-coordinate for the current violin column is i
        # Mark Min and Max (Red triangles)
        plt.scatter([i, i], [cmin, cmax], color='red', marker='^', zorder=3, label='Min/Max' if i == 0 else "")

        # Mark Mean (Blue circle)
        plt.scatter(i, cavg, color='blue', marker='o', s=50, zorder=3, label='Mean' if i == 0 else "")

        # Mark Standard Deviation range (Green vertical line: Mean +/- 1 StdDev)
        plt.vlines(i, cavg - cdev, cavg + cdev, color='green', linewidth=4, zorder=2, label='1 StdDev' if i == 0 else "")

    if title:
        plt.title(title)
    if ylabel:
        plt.ylabel(ylabel)

    plt.legend(loc='lower left')
    
