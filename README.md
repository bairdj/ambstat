# AmbStat
A dashboard application to visualise open statistics about English NHS ambulance services. AmbStat was created using R.
All data is sourced from [NHS England Ambulance Quality Indicators](https://www.england.nhs.uk/statistics/statistical-work-areas/ambulance-quality-indicators/).
## Included statistics
* Cardiac arrest ROSC and survival over time
* Cardiac arrest outcome flow
* STEMI perfomance
* Hear & Treat rates
* See & Treat rates
* Call response times against NHS targets
* Compare ambulance services
### Docker
AmbStat has a Docker build which serves the application on port 80 using a Shiny Server instance.
The application can be quickly started by running
`
docker run --rm -p 80:80 bairdj/ambstat
`
