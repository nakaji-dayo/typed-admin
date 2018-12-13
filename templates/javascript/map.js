var map;
function initMap() {
  var textLocationLat = document.getElementById("locationLat")
  var textLocationLon = document.getElementById("locationLon")
  var initialLocation = {lat: parseFloat(textLocationLat.value),
                         lng: parseFloat(textLocationLon.value)}
  map = new google.maps.Map(document.getElementById('map'), {
    center: initialLocation,
    zoom: 8
  });
  google.maps.event.addListener(map, 'click', function(event) {
    placeMarker(event.latLng);
  });
  var marker = new google.maps.Marker({
    position: initialLocation,
    map: map
  });
  function placeMarker(location) {
    console.log('location', location)
    marker.setPosition(location)
    textLocationLat.value = location.lat()
    textLocationLon.value = location.lng()
  }
}
