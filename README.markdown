#eAMF

eAMF provides Action Message Format (AMF) support for Erlang, compatible with Adobe's Flash Player

**Note: this library is a work in progress and not ready for use yet see the [Issues Page](http://github.com/mrinalwadhwa/eAMF/issues) for current bugs and planned featuers**


##Action Message Format (AMF)

[Action Message Format][amf3l] is a compact binary format that is used to serialize ActionScript object graphs. Once serialized an AMF encoded object graph may be used to persist and retrieve the public state of an application across sessions or allow two endpoints to communicate through the exchange of strongly typed data.


##Features

eAMF currently supports AMF3 ([Specification][amf3l]) and provides two main features
	
**Serialization:**
<code>amf3:encode/1</code> function can be used to convert Erlang terms to binary AMF3 encoded data    
**De-Serialization:**
<code>amf3:decode/1</code> function can be used to convert binary AMF3 encoded data to Erlang terms    


##Types

###undefined
Actionscript's [undefined](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#undefined) value corresponds to Erlang [atom][al] undefined

---

###null
Actionscript's [null](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/statements.html#null) value corresponds to Erlang [atom][al] null

---

###Boolean
Actionscript's [Boolean](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Boolean.html) type corresponds to [bool()][bl] in Erlang which is essentially the [atoms][al] true and false 

---

###Number
* All integer values in Actionscript from -268435456 to 268435455, regardless of the type of variable they are stored in - [int](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/int.html), [uint](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/uint.html) or [Number](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Number.html) correspond to an [integer()](http://www.erlang.org/doc/reference_manual/data_types.html#id2259119) value in Erlang

* All integer values in Actionscript from -1.79e308 to -268435457 and from 268435456 to 1.79e308, regardless of the type of variable they are stored in - [int](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/int.html), [uint](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/uint.html) or [Number](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Number.html) correspond to [float()](http://www.erlang.org/doc/reference_manual/data_types.html#id2259119) in Erlang 

* All floating point values in Actionscript correspond to [float()](http://www.erlang.org/doc/reference_manual/data_types.html#id2259119) in Erlang 

* Actionscript’s [Infinity](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#Infinity) value corresponds to Erlang [atom][al] infinity

* Actionscript’s [-Infinity](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#-Infinity) value corresponds to Erlang [atom][al] '-infinity'

* Actionscript’s [NaN](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/package.html#NaN) value corresponds to Erlang [atom][al] nan



--- 

###String
An Actionscript String corresponds to a [binary() string][binl] in Erlang.

**Note:** eAMF deals with Erlang strings in binary form eg. <<"hello world">> and not as a List eg. "hello world". Passing the <code>amf3:encode/1</code> a List will result in an Actionscript Array and not an Actionscript String

--- 

###XML
Actionscript's [XML](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/XML.html) type corresponds to an Erlang [tuple](http://www.erlang.org/doc/reference_manual/data_types.html#id2262011) of the form {xml, XMLString}, where XMLString is the xml represented as a string in [binary()][binl] form.


--- 

###XMLDocument
Actionscript's [XMLDocument](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/xml/XMLDocument.html) type corresponds to an Erlang  [tuple](http://www.erlang.org/doc/reference_manual/data_types.html#id2262011) of the form {xmldoc, XMLDocString}, where XMLDocString is the xml represented as a string in [binary()][binl] form.

--- 

###Date
Actionscript's [Date](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Date.html) type corresponds to an Erlang [tuple](http://www.erlang.org/doc/reference_manual/data_types.html#id2262011) of the form {date, Milliseconds}, where Milliseconds is the time in milliseconds that has elapsed since midnight January 1, 1970, universal time (UTC)


--- 

###Array

An Actionscript [Array](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/Array.html) corresponds to an Erlang [list()](http://www.erlang.org/doc/reference_manual/data_types.html#id2265282) 


--- 

###Object
An Actionscript Object corresponds to an Erlang [tuple](http://www.erlang.org/doc/reference_manual/data_types.html#id2262011) of the form {object, ClassName, Members} where ClassName is a string in [binary()][binl] form that represents the class name of this object and Members is a [dictionary()](http://www.erlang.org/doc/man/dict.html) where property names are keys


--- 

###ByteArray
Actionscript's [ByteArray](http://help.adobe.com/en_US/FlashPlatform/reference/actionscript/3/flash/utils/ByteArray.html) type corresponds to an Erlang [tuple](http://www.erlang.org/doc/reference_manual/data_types.html#id2262011) of the form {bytearray, Bytes}, where Bytes is [binary()][binl]



##Examples
eAMF has a comprehensive set of unit tests, looking at them would help - [see here]()


##LICENSE
eAMF is licensed under the MIT License (see [LICENSE file](http://github.com/mrinalwadhwa/eAMF/blob/master/LICENSE) for more details)



[amf3l]: http://bit.ly/amf-spec
[al]: http://www.erlang.org/doc/reference_manual/data_types.html#id2259149
[bl]: http://www.erlang.org/doc/reference_manual/data_types.html#id2257962
[binl]: http://www.erlang.org/doc/reference_manual/data_types.html#id2252522