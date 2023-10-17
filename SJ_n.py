import pandas as pd
from darts import TimeSeries
from darts.models import BlockRNNModel, NBEATSModel, TransformerModel, TCNModel

csv_file = 'SanJuan_data_weekly.csv'
df = pd.read_csv(csv_file)
time_index = df.index

cases_values = df['Cases'].values
cases_series = TimeSeries.from_values(values=cases_values)

rain_values = df['Rain'].values
rain_series = TimeSeries.from_values(values=rain_values)

n_values = [52, 48, 44, 40, 36, 32, 28, 24, 20, 16, 12, 8, 4]

for n in n_values:
    test_data = df[-n:]
    train_data = df[:-n]

    train_series = TimeSeries.from_dataframe(train_data[['Cases']])
    test_series = TimeSeries.from_dataframe(test_data[['Cases']])
    train_covariate_series = TimeSeries.from_dataframe(train_data[['Rain']])
    test_covariate_series = TimeSeries.from_dataframe(test_data[['Rain']])

    # BlockRNN Model
    block_rnn_model = BlockRNNModel(input_chunk_length=30, output_chunk_length=13, n_rnn_layers=2)
    block_rnn_model.fit(train_series, past_covariates=rain_series, epochs=100, verbose=True)
    block_rnn_pred = block_rnn_model.predict(n)

    # NBEATS Model
    model_nbeats = NBEATSModel(input_chunk_length=20, output_chunk_length=13, random_state=42)
    model_nbeats.fit(train_series, past_covariates=rain_series, epochs=100, verbose=True)
    nbeats_pred = model_nbeats.predict(n)

    # Transformer Model
    my_tf_model = TransformerModel(
        input_chunk_length=15,
        output_chunk_length=13,
        batch_size=32,
        n_epochs=100,
        model_name="transformer",
        nr_epochs_val_period=10,
        d_model=16,
        nhead=8,
        num_encoder_layers=2,
        num_decoder_layers=2,
        dim_feedforward=128,
        dropout=0.1,
        activation="relu",
        random_state=42,
        save_checkpoints=True,
        force_reset=True,
    )
    my_tf_model.fit(series=train_series, past_covariates=rain_series, verbose=True)
    tf_pred = my_tf_model.predict(n)

    # TCN Model
    deep_tcn = TCNModel(
        input_chunk_length=20,
        output_chunk_length=13,
        kernel_size=2,
        num_filters=4,
        dilation_base=2,
        dropout=0,
        random_state=0,
    )
    deep_tcn.fit(train_series, past_covariates=rain_series, verbose=True)
    tcn_pred = deep_tcn.predict(n)

