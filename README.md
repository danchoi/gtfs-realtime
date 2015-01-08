# gtfs-realtime

Haskell code to parse GTFS realtime protocol buffers. Target data source is the
Boston MBTA.

This is not production ready.

## Sample usage

```bash
curl http://developer.mbta.com/lib/GTRTFS/Alerts/TripUpdates.pb | dist gtfs-realtime t
curl http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb | dist gtfs-realtime v
curl http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb | dist gtfs-realtime a
```


## Related Documentation

https://developers.google.com/transit/gtfs-realtime/reference#TripUpdate

https://hackage.haskell.org/package/protobuf-0.2.0.4/docs/Data-ProtocolBuffers.html

http://realtime.mbta.com/Portal/Home/Documents

http://realtime.mbta.com/Portal/Content/Documents/MBTA-realtime_GTFSRTDocumentation_v2_2014-08-04.pdf
MBTA endpoints

http://developer.mbta.com/lib/GTRTFS/Alerts/VehiclePositions.pb

http://developer.mbta.com/lib/GTRTFS/Alerts/TripUpdates.pb

http://developer.mbta.com/lib/GTRTFS/Alerts/Alerts.pb

https://code.google.com/p/protobuf-haskell/wiki/Basics

https://github.com/dcodeIO/ProtoBuf.js/wiki/ProtoBuf.js-vs-JSON


