enum DistanceT { FEET, METERS, MILES };
struct DataT {
    float distance;
    enum DistanceT distanceType;
    float seconds;
    };

float miles_per_hour ( struct DataT data ) {
   float miles = data.distance;
   switch ( data.distanceType ) {
      case METERS:
         miles = data.distance / 1609.344;
         break;
      case FEET:
         miles = data.distance / 5280.0;
         break;
   };
   return miles / ( data.seconds / ( 60.0 * 60.0 ) );
}
