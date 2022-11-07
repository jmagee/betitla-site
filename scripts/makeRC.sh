#!/bin/sh
# Create the Betitla RC file..

output=$1
cat > $output << BETITLA
{ "ElevationRating.terms" : "$APP_HOME/betitla-data/Terms/ElevationRating.terms"
, "DistanceRating.terms"  : "$APP_HOME/betitla-data/Terms/DistanceRating.terms"
, "DurationRating.terms"  : "$APP_HOME/betitla-data/Terms/DurationRating.terms"
, "PhaseOfDay.terms"      : "$APP_HOME/betitla-data/Terms/PhaseOfDay.terms"
, "SpeedRating.terms"     : "$APP_HOME/betitla-data/Terms/SpeedRating.terms"
, "Sport.terms"           : "$APP_HOME/betitla-data/Terms/Sport.terms"
, "Client.secret"         : "$BETITLA_CLIENT_SECRET"
, "App.id"                : "$BETITLA_APP_ID"
, "Db.name"               : "$APP_HOME/betitla-data/betitla.db"
, "App.slogan"            : "This activity's title brought to you by Betitla - the entitled renamer of things"
, "Host.url"              : "https://betitla.cleverapps.io/thanks/"
}
BETITLA
