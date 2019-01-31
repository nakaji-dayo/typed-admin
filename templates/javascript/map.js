var map;
var marker;
var textLocationLat;
var textLocationLon;
function initMap() {
  textLocationLat = document.getElementById("locationLat")
  textLocationLon = document.getElementById("locationLon")
  var initialLocation = {lat: parseFloat(textLocationLat.value),
                         lng: parseFloat(textLocationLon.value)}
  map = new google.maps.Map(document.getElementById('map'), {
    center: initialLocation,
    zoom: 13
  });
  google.maps.event.addListener(map, 'click', function(event) {
    placeMarker(event.latLng);
  });
  marker = new google.maps.Marker({
    position: initialLocation,
    map: map
  });
}
function placeMarker(location) {
  console.log('location', location)
  marker.setPosition(location)
  textLocationLat.value = location.lat()
  textLocationLon.value = location.lng()
  map.setCenter({lat: location.lat(), lng: location.lng()});
}
