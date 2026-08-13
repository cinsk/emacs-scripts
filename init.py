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

