#
# Probability assignment 2023
# 		 Name: Liam Flynn
# 	Programme: Applied/Games Development


studentID = 20098690

q1 = function()
{
	#  A pizzeria has three categories of pizza toppings, 9 are meat, 12 are seafood and 11 are 
	# vegetable. A particular offer allows customers to choose 8 different toppings. If all toppings 
	# are equally likely to be selected, how likely is it that the next such pizza ordered will have
	# specified exactly 2 meat toppings?

	exactP = dbinom(2,8,9/32)

	count = 0
	for(i in 1:200){
	sim200 = sample(32,8,replace == FALSE) #choses from 1 to 32 8 times without replacing chosen numbers
	meatChosen = sim200[sim200 <=9] #how many times numbers less than or equal to 9 are chosen (meat)
	if(meatChosen==2){
	count = count +1
}
}
	estimateP = count/200
	
	
	# Return two values
	c(exactP, estimateP)
}

q2 = function()
{
	# Correct values for exactP and estimateP to be determined
	exactP = -1
	estimateP = -1
	
	
	# Return two values
	c(exactP, estimateP)
}
q3 = function()
{
	# Correct values for exactP and estimateP to be determined
	exactP = -1
	estimateP = -1
	
	
	# Return two values
	c(exactP, estimateP)
}

q4 = function()
{
	# Minor accidents (burns, cuts etc.) occur from time to time in the preparation are of the pizzeria 
	#and need to be recorded in an accident log. These can be modelled with a Poisson distribution
	#with the average time between accidents being 16 hours 30 minutes. Calculate the probability 
	#that the number of minor accidents in a 61 hour period of operation will be at least 4.
	
	#At least 4 = Greater than 3
	#1 accident per 16.5 hours -> 1/16.5 accident per hour -> 1/16.5 * 61 -> 3.69697

	exactP = 1 - ppois(3,3.69697)
	
	fourAccs = rpois(200,3.69697)
	probFourAccs  = length(fourAccs[fourAccs>3])
	estimateP = probFourAccs / 200
	
	# Return two values
	c(exactP, estimateP)
}


q5 = function()
{
	# Correct values for exactP and estimateP to be determined
	exactP = -1
	estimateP = -1
	
	
	# Return two values
	c(exactP, estimateP)
}