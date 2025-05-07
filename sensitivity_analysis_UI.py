#####
#
# To run this make sure you have the following installed:
# - streamlit (pip install streamlit)
# Then run via terminal using the command:
# streamlit run sensitivity_analysis_UI.py
#
#####


import streamlit as st
import pandas as pd
import matplotlib.pyplot as plt

# -- Load data once --
@st.cache_data
def load_factors():
    df = pd.read_csv('computed_factors.csv')
    df['Date'] = pd.to_datetime(df['Date']).dt.tz_localize(None)
    return df

@st.cache_data
def load_prices(horizon_days):
    price_files = {
        'CIMB MK Equity': 'Data_Market_CIMB.xlsx',
        'GAM MK Equity':  'Data_Market_GAM.xlsx',
    }
    dfs = []
    for ticker, fn in price_files.items():
        p = pd.read_excel(fn, skiprows=1, na_values=['#N/A'])
        p['Date'] = pd.to_datetime(p['Date'], dayfirst=True)
        p = p.dropna(subset=['PX_LAST']).sort_values('Date')
        p['Ticker'] = ticker
        p['forward_return'] = p['PX_LAST'].shift(-horizon_days) / p['PX_LAST'] - 1
        dfs.append(p[['Ticker','Date','forward_return']])
    return pd.concat(dfs, ignore_index=True)

# Title
st.title("Sensitivity Analysis")

# Load data
factors = load_factors()

# Sidebar controls
st.sidebar.header("Parameters")
# Date range
min_date, max_date = factors['Date'].min().date(), factors['Date'].max().date()
start_date = st.sidebar.date_input('Start Date', min_date, min_value=min_date, max_value=max_date)
end_date = st.sidebar.date_input('End Date', max_date, min_value=min_date, max_value=max_date)

# Ticker selection
tickers = sorted(factors['Ticker'].unique())
sel_ticker = st.sidebar.selectbox('Ticker', tickers)

# Factor selection
factors_list = sorted(factors['Name'].unique())
sel_factor = st.sidebar.selectbox('Factor', factors_list)

# Horizon selection
horizon_map = {
    '1 day': 1,         # 1 trading day
    '1 week': 5,        # 5 trading days in a typical week
    '1 month': 21,      # ~21 trading days in a month
    '1 quarter': 63,    # ~63 trading days in a quarter (3 months)
    '1 year': 252       # ~252 trading days in a year
}

sel_horizon_label = st.sidebar.selectbox('Horizon', list(horizon_map.keys()))
sel_horizon = horizon_map[sel_horizon_label]

# Analyze button
def run_analysis():
    # Filter factors
    df_fact = factors[(factors['Ticker']==sel_ticker) & (factors['Name']==sel_factor)].copy()
    df_fact = df_fact[(df_fact['Date'].dt.date>=start_date) & (df_fact['Date'].dt.date<=end_date)]
    # Load prices and merge
    prices = load_prices(sel_horizon)
    df_plot = df_fact.merge(prices[prices['Ticker']==sel_ticker], on=['Ticker','Date'], how='inner')
    df_plot = df_plot.dropna(subset=['forward_return', 'Value'])

    # Plot
    fig, ax = plt.subplots(figsize=(8,6))
    ax.scatter(df_plot['Value'], df_plot['forward_return']*100, alpha=0.7)
    ax.set_title('Sensitivity Analysis')
    ax.set_xlabel(sel_factor)
    ax.set_ylabel(f"{sel_horizon_label} Forward Return (%)")
    ax.grid(True)
    st.pyplot(fig)

if st.sidebar.button('Analyze'):
    run_analysis()
else:
    st.info('Adjust parameters in the sidebar and click Analyze')
