\name{taxi}
\alias{taxi}
\docType{data}
\title{NYC taxi sample data.}
\description{
Trips made by New York City?셲 iconic yellow taxis have been recorded and provided to the TLC since 2009. Yellow taxis are traditionally hailed by signaling to a driver who is on duty and seeking a passenger (street hail), but now they may also be hailed using an e-hail app like Curb or Arro. Yellow taxis are the only vehicles permitted to respond to a street hail from a passenger in all five boroughs.
Records include fields capturing pick-up and drop-off dates/times, pick-up and drop-off locations, trip distances, itemized fares, rate types, payment types, and driver-reported passenger counts. The records were collected and provided to the NYC Taxi and Limousine Commission (TLC) by technology service providers. The trip data was not created by the TLC, and TLC cannot guarantee their accuracy
}
\usage{
head(taxi)
}
\format{
  A data frame with 479,706 observations on the following 21 variables.
  \describe{
    \item{\code{medalian}}{character. A permit to operate a yellow taxi cab in New York City, it is effectively a (randomly assigned) car ID}
    \item{\code{hack_license}}{character. A license to drive the vehicle, it is effectively a (randomly assigned) driver ID}
    \item{\code{vendor_id}}{factor. A code indicating the TPEP provider that provided the record.
    1= Creative Mobile Technologies,LLC
    2= VeriFone Inc.}
    \item{\code{payment_type}}{factor. A numeric code signifying how the passenger paid for the trip. 1= Credit card, 2= Cash, 3= No charge, 4=Dispute, 5= Unknown, 6= Voided trip}
    \item{\code{fare_amount}}{numeric. The time-and-distance fare calculated by the meter.}
    \item{\code{surcharge}}{numeric. : Extra fees, such as rush hour and overnight surcharges. }
    \item{\code{mta_tax}}{numeric. $0.50 MTA tax that is automatically triggered based on the metered rate in use.}
    \item{\code{tip_amount}}{numeric. Tip amount ?? This field is automatically populated for credit card tips. Cash tips are not included.}
    \item{\code{tolls_amount}}{numeric. Total amount of all tolls paid in trip.}
    \item{\code{total_amount}}{numeric. The total amount charged to passengers. Does not include cash tips.}
    \item{\code{rate_code}}{factor. The final rate code in effect at the end of the trip. 1= Standard rate, 2=JFK, 3=Newark, 4=Nassau or Westchester, 5=Negotiated fare, 6=Group ride}
    \item{\code{store_and_fwd_flag}}{factor. This flag indicates whether the trip record was held in vehicle memory before sending to the vendor, aka ?쐓tore and forward,?? because the vehicle did not have a connection to the server. Y= store and forward trip, N= not a store and forward trip}
    \item{\code{pickup_datetime}}{character. start time of the trip, mm-dd-yyyy hh24:mm:ss EDT}
    \item{\code{dropoff_datetime}}{character. end time of the trip, mm-dd-yyyy hh24:mm:ss EDT}
    \item{\code{passenger_count}}{integer. The number of passengers in the vehicle. This is a driver-entered value.}
    \item{\code{trip_time_in_secs}}{integer. trip time measured by the taximeter in seconds.}
    \item{\code{trip_distance}}{numeric. The elapsed trip distance in miles reported by the taximeter.}
    \item{\code{pickup_longitude, pickup_latitude}}{numeric. GPS coordinates at the start of the trip}
    \item{\code{dropoff_longitude, dropoff_latitude}}{numeric. GPS coordinates at the end of the trip}
  }
}
\source{
https://www1.nyc.gov/site/tlc/about/tlc-trip-record-data.page
}
\examples{
data(taxi)
str(taxi)
}
\keyword{NYC Taxi}
