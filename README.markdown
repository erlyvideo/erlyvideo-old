#eAMF

eAMF provides Action Message Format (AMF) support for Erlang, compatible with Adobe's Flash Player

**Note: this library is a work in progress and not ready for use yet see the [Issues Page](http://github.com/mrinalwadhwa/eAMF/issues) for current bugs and planned featuers**


##Action Message Format (AMF)

[Action Message Format](http://bit.ly/amf-spec) is a compact binary format that is used to serialize ActionScript object graphs. Once serialized an AMF encoded object graph may be used to persist and retrieve the public state of an application across sessions or allow two endpoints to communicate through the exchange of strongly typed data.


##Features

eAMF currently supports AMF3 ([Specification](http://bit.ly/amf-spec)) and provides two main features
	
**Serialization:**
<code>amf3:encode/1</code> function can be used to convert Erlang terms to binary AMF3 encoded data    
**De-Serialization:**
<code>amf3:decode/1</code> function can be used to convert binary AMF3 encoded data to Erlang terms    


##How to build?

eAMF uses [GNU Make](http://www.gnu.org/software/make/) for building binaries from source.  

<code>$ make</code>
	
1. Builds the source
2. Executes eunit unit tests
3. Creates documentation using edoc


<code>$ make debug</code>
	
1. Builds the source in debug mode
2. Executes eunit unit tests
3. Runs code coverage analysis using the cover module
4. Analyzes the code using dialyzer


##Types

###undefined
Actionscript's [undefined](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#undefined) value corresponds to Erlang [atom](http://www.erlang.org/doc/reference_manual/data_types.html#id2259149) undefined

---

###null
Actionscript's [null](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/statements.html#null) value corresponds to Erlang [atom](http://www.erlang.org/doc/reference_manual/data_types.html#id2259149) null

---

###Boolean
Actionscript's [Boolean](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Boolean.html) type corresponds to [bool()](http://www.erlang.org/doc/reference_manual/data_types.html#id2257962) in Erlang which is essentially the [atoms](http://www.erlang.org/doc/reference_manual/data_types.html#id2259149) true and false 

---

###Numbers
* All integer values in Actionscript from -268435456 to 268435455, regardless of the type of variable they are stored in - [int](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/int.html), [uint](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/uint.html) or [Number](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Number.html) correspond to an [integer()](http://www.erlang.org/doc/reference_manual/data_types.html#id2259119) value in Erlang

* All integer values in Actionscript from -1.79e308 to -268435457 and from 268435456 to 1.79e308, regardless of the type of variable they are stored in - [int](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/int.html), [uint](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/uint.html) or [Number](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Number.html) correspond to [float()](http://www.erlang.org/doc/reference_manual/data_types.html#id2259119) in Erlang 

* All floating point values in Actionscript correspond to [float()](http://www.erlang.org/doc/reference_manual/data_types.html#id2259119) in Erlang 

* Actionscript’s [Infinity](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#Infinity) value corresponds to Erlang [atom](http://www.erlang.org/doc/reference_manual/data_types.html#id2259149) infinity

* Actionscript’s [-Infinity](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#-Infinity) value corresponds to Erlang [atom](http://www.erlang.org/doc/reference_manual/data_types.html#id2259149) '-infinity'

* Actionscript’s [NaN](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#NaN) value corresponds to Erlang [atom](http://www.erlang.org/doc/reference_manual/data_types.html#id2259149) 'nan'

--- 


##Examples
eAMF has a comprehensive set of unit tests, looking at them would help 


##LICENSE
eAMF is licensed under the MIT License (see [LICENSE file](http://github.com/mrinalwadhwa/eAMF/blob/master/LICENSE) for more details)

