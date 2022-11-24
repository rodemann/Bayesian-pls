import tensorflow as tf
import tensorflow_probability as tfp
import numpy as np

# fuck up with cuda rn => no gpu support
tf.test.is_gpu_available()
tf.config.list_physical_devices('GPU')

# test TF via python call 
# intro by this dude: https://towardsdatascience.com/introduction-to-tensorflow-probability-6d5871586c0e
aa = tf.constant([[1.0, 2.0], [3.0, 4.0]])

tfd = tfp.distributions
binomial_dist = tfd.Binomial(total_count=1000, probs=0.3)
binomial_dist.sample(3)

def create_dataset(n, x_range, slope=2, intercept=10, noise=0.5):
    x_uniform_dist = tfd.Uniform(low=x_range[0], high=x_range[1])
    x = x_uniform_dist.sample(n).numpy().reshape(-1, 1)
    y_true = slope*x+intercept
    eps_uniform_dist = tfd.Normal(loc=0, scale=1)
    eps = eps_uniform_dist.sample(n).numpy().reshape(-1, 1)*noise*x
    y = y_true + eps
    return x, y, y_true
    
n_train = 5000
n_val = 1000
n_test = 5000
x_range = [-10, 10]
x_train, y_train, y_true = create_dataset(n_train, x_range)
x_val, y_val, _ = create_dataset(n_val, x_range)
x_test = np.linspace(x_range[0], x_range[1], n_test).reshape(-1, 1)

tfkl = tf.keras.layers
# Model Architecture
model = tf.keras.Sequential([
    tfkl.Dense(1, input_shape = (1,))
])
# Model Configuration
model.compile(optimizer=tf.optimizers.Adam(learning_rate=0.01), loss=tf.keras.losses.MeanSquaredError())
# Early Stopping Callback
callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=30, min_delta=0, mode='auto', baseline=None, restore_best_weights=True)
# Model Fitting
history = model.fit(x_train, y_train, validation_data=(x_val, y_val), epochs=100, verbose=True, shuffle=True, callbacks=[callback], batch_size = 100)

## BNN
tfpl = tfp.layers
model = tf.keras.Sequential([
    tfkl.Dense(2, input_shape = (1,)),
    tfpl.DistributionLambda(lambda t: tfd.Normal(loc=t[..., :1], scale=1e-3+tf.math.abs(t[...,1:])))
])

negloglik = lambda y_true, y_pred: -y_pred.log_prob(y_true)
model.compile(optimizer=tf.optimizers.Adam(learning_rate=0.01), loss=negloglik)

# Early Stopping Callback
callback = tf.keras.callbacks.EarlyStopping(monitor='val_loss', patience=300, min_delta=0, mode='auto', baseline=None, restore_best_weights=True)
# Model Fitting
history = model.fit(x_train, y_train, validation_data=(x_val, y_val), epochs=100, verbose=True, shuffle=True, callbacks=[callback], batch_size = 100)
y_pred_mean = model(x_test).mean()
y_pred_std = model(x_test).stddev()