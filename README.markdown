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


##Examples
eAMF has a comprehensive set of unit tests, looking at them would help 


##LICENSE
eAMF is licensed under the MIT License (see [LICENSE file](http://github.com/mrinalwadhwa/eAMF/blob/master/LICENSE) for more details)

