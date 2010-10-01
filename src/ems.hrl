

-define(RTMP_PORT,     1935).
-define(TIMEOUT,     120000).
-define(PREPUSH, 3000).
-define(RTMP_WINDOW_SIZE, 2500000).
-define(RTMP_PREF_CHUNK_SIZE, (1024*1024)).

-define(CONTENT_TYPE, "application/x-fcs").






% AMF commands
-define(AMF_COMMAND_ONMETADATA, "onMetaData").
-define(AMF_COMMAND_ONCUEPOINT, "onCuePoint").


%% NetConnection Status
-define(NC_CALL_FAILED, "NetConnection.Call.Failed").
-define(NC_CALL_BAD_VERSION, "NetConnection.Call.BadVersion").
-define(NC_CONNECT_APP_SHUTDOWN, "NetConnection.Connect.AppShutdown").
-define(NC_CONNECT_CLOSED, "NetConnection.Connect.Closed").
-define(NC_CONNECT_FAILED, "NetConnection.Connect.Failed").
-define(NC_CONNECT_REJECTED, "NetConnection.Connect.Rejected").
-define(NC_CONNECT_SUCCESS, "NetConnection.Connect.Success").
-define(NC_CONNECT_INVALID_APP, "NetConnection.Connect.InvalidApp").

%% NetStream Status
-define(NS_INVALID_ARG, "NetStream.InvalidArg").
-define(NS_CLEAR_SUCCESS, "NetStream.Clear.Success").
-define(NS_CLEAR_FAILED, "NetStream.Clear.Failed").
-define(NS_PUBLISH_BAD_NAME, "NetStream.Publish.BadName").
-define(NS_FAILED, "NetStream.Failed").
-define(NS_UNPUBLISHED_SUCCESS, "NetStream.Unpublish.Success").
-define(NS_RECORD_START, "NetStream.Record.Start").
-define(NS_RECORD_NO_ACCESS, "NetStream.Record.NoAccess").
-define(NS_RECORD_STOP, "NetStream.Record.Stop").
-define(NS_RECORD_FAILED, "NetStream.Record.Failed").
-define(NS_PLAY_INSUFFICIENT_BW, "NetStream.Play.InsufficientBW").
-define(NS_PLAY_START, "NetStream.Play.Start").
-define(NS_PLAY_STREAM_NOT_FOUND, "NetStream.Play.StreamNotFound").
-define(NS_PLAY_STOP, "").
-define(NS_PLAY_FAILED, "NetStream.Play.Failed").
-define(NS_PLAY_RESET, "NetStream.Play.Reset").
-define(NS_PLAY_PUBLISH_NOTIFY, "NetStream.Play.PublishNotify").
-define(NS_PLAY_UNPUBLISH_NOTIFY, "NetStream.Play.UnpublishNotify").
-define(NS_PLAY_SWITCH, "NetStream.Play.Switch").
-define(NS_PLAY_COMPLETE, "NetStream.Play.Complete").
-define(NS_SEEK_NOTIFY, "NetStream.Seek.Notify").
-define(NS_SEEK_FAILED, "NetStream.Seek.Failed").
-define(NS_PAUSE_NOTIFY, "NetStream.Pause.Notify").
-define(NS_UNPAUSE_NOTIFY, "NetStream.Unpause.Notify").
-define(NS_DATA_START, "NetStream.Data.Start").

%% Application Status
-define(APP_SCRIPT_ERROR, "Application.Script.Error").
-define(APP_SCRIPT_WARNING, "Application.Script.Warning").
-define(APP_RESOURCE_LOW_MEMORY, "Application.Resource.LowMemory").
-define(APP_SHUTDOWN, "Application.Shutdown").
-define(APP_GC, "Application.GC").

%% Shared Object Status
-define(SO_NO_READ_ACCESS, "SharedObject.NoReadAccess").
-define(SO_NO_WRITE_ACCESS, "SharedObject.NoWriteAccess").
-define(SO_OBJECT_CREATION_FAILED, "SharedObject.ObjectCreationFailed").
-define(SO_BAD_PERSISTENCE, "SharedObject.BadPersistence").



-record(file_frame, {
  id,
  dts,
  pts,
  type,
  offset,
  size,
  keyframe
}).

		
