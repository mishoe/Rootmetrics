source('IndividualScoreScript.R')

address = "105 Lakeworth Drive, Columbia, Sc"
carrier='Verizon'
radius_miles=30

output = generate_individ_scores(targ_locat = address,carrier=carrier,radius_miles = radius_miles)
  