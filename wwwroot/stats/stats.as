import flash.events.MouseEvent;
import flash.net.NetConnection;
import flash.net.NetStream;
import mx.collections.*;
import mx.utils.ObjectUtil;

private var startTime:uint = 1283427840;
private var server:String = "rtmp://localhost/rtmp";
private var stream:String = "cam0_view";

[Bindable]
public var entries:ArrayCollection;

[Bindable]
public var common_stats:ArrayCollection;

[Bindable]
public var entries_label:String = "Entries";

[Bindable]
public var entry_count_text:String = "Hi";

[Bindable]
public var users_label:String = "Users";

[Bindable]
public var users:ArrayCollection;

[Bindable]
public var debug_text:String = "Hi";

[Bindable]
public var rtmp_trafic:ArrayCollection;

private var nc:NetConnection;
private var ns:NetStream;

private var seekTo:uint;
private var interval:uint; 

internal function completeCreation():void
{
  callConnect();
}
  
private function callConnect():void
{
  nc = new NetConnection();
	nc.addEventListener(NetStatusEvent.NET_STATUS, netStatus);
	nc.connect(server);
}

private function loadStats():void
{
	var responder:Responder = new Responder(onEntriesLoaded, null);
	
	nc.call("entries", responder);
}
    
private function netStatus(event:NetStatusEvent):void {
  switch(event.info.code) {
    case 'NetConnection.Connect.Success': {
  		loadStats();
  		nc.call("subscribeStats", null)
		
  		ns = new NetStream(nc);
  		ns.client = this;
  		ns.bufferTime = 2;
		
  		interval = setInterval(mon,1000);
  		setInterval(showTime,50);
  		break;
    };
    case 'NetConnection.Message': {
      var info:XML = new XML(event.info.description);
      debug_text = info;
      loadStats();
      break;
    }
  }
}

public function onEntriesLoaded(result:Object):void {
  var info:Array = [
    {Param : 'avg1', Value: result.cpu.avg1},
    {Param : 'avg5', Value: result.cpu.avg5},
    {Param : 'avg15', Value: result.cpu.avg15}
  ];
  entry_count_text = "Length: "+result.streams.length;
  common_stats = new ArrayCollection(info);
  entries = new ArrayCollection(result.streams);
  
  rtmp_trafic = new ArrayCollection(result.rtmp);
  
  entries_label = "Streams ("+entries.length+")";
  
  
  users_label = "Users ("+result.users.length+")";
  users = new ArrayCollection(result.users);
}

public function onPlayStatus(info:Object):void {
	
}

public function onLastSecond(info:Object):void {
	
}

public function onMetaData(info:Object):void {
	trace("metadata: duration=" + info.duration + " width=" + info.width + " height=" + info.height +
		" framerate=" + info.framerate); 
}

public function onCuePoint(info:Object):void {
	trace("cuepoint: time=" + info.time + " name=" + info.name + " type=" + info.type);
}

public function mon():void {
	trace("Buffer length: " + ns.bufferLength + " Time: " + ns.time);
	//trace("FPS:"+ns.currentFPS);
}

public function showTime():void {
}

protected function button_resume(event:MouseEvent):void
{
	ns.resume();
}

protected function button_pause(event:MouseEvent):void
{
	ns.pause();
}

protected function button_seek(time:Number):void
{	
	seekTo = startTime + ns.time + time; 
	ns.seek(seekTo);
	trace("seek to: " + seekTo);
}
