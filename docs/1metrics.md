# metric functions

<code>(metric model attributes data)</code>

<dl>
<dt><code>metric</code></dt> 
<dd>a function that returns a double. An example is mean absolute error for a 
regression model.
</dd>

<dt><code>model</code></dt> 
<dd>a function <code>(model predictors datum)</code> that returns a prediction
for the given <code>datum</code>.

<dt><code>attributes</code></dt>
<dd>a map from keyword names to attribute functions, like:<br>
<pre>
{:height #object[foo$height 0x7b993c65 "foo$height@7b993c65"] 
 :width #object[foo$width 0x407a7f2a "foo$width@407a7f2a"] 
 ...
 :ground-truth #object[foo$defect 0x7b993c65 "foo$defect@7b993c65"]}
</pre> 
The map must have a <code>:ground-truth</code> key-value pair,
and must include key-value pairs for every predictor attribute used by
model.
<br>
Attribute function calls in training, prediction, and metrics look like 
<code>((:height attributes) datum)</code>.
<br>
The map provides indirection used in serializing models and in computing 
permutation-based importance statistics.
<br>
This odd api is driven by the needs of taiga.permutation, which is driven by
the questionable choice of model function + predictor map + data collection,
which allows serialization without a direct reference to predictor functions
and redefining the predictor functions with shuffled versions for permutation
statistics. 
<br>
An alternative would be to figure out how to reliably serialize 
possibly anonymous local functions, and also require models to offer an
update operation that returns a new model with all references to function A
replaced by function B.
</dd>

<dt><code>data</code></dt>
<dd>an <code>Iterable</code> whose elements are suitable for passing to the predictor and 
<code>ground-truth</code> functions.
</dd>
</dl>