# To run SCS1_2.Rd, enter "Rscript SCS1_2.Rd" into the command line.
# R may need to be installed if it is not already.
# For the visual display mentioned in my PDF file, see the commenting in the "greedySCS"
# funtion below.
# added line of comment

# String Set 1

s0  = "babbabbababba"
s1  = "bbabbababbabb"
s2  = "ababbabbababb"
s3  = "abbababbabbab"
s4  = "bababbababbab"
s5  = "babbababbabab"
s6  = "babbabbababba"
s7  = "bbababbabbaba"
s8  = "ababbababbabb"
s9  = "abbababbababb"
s10  = "abbabbababbab"
s11  = "bababbabbabab"
s12  = "babbababbabba"
s13  = "babbabbababba"
s14  = "bbababbabbaba"
s15  = "ababbababbabb"
s16  = "abbababbababb"
s17  = "abbabbababbab"
s18  = "bababbabbabab"
s19  = "babbababbabba"


# Vector S1 contains all unique strings above.
S1 = c(s0, s1, s2, s3, s4, s5, s7, s8, s9, s10, s11, s12)

# String Set 2

S2 = c(letters[1:20]) 

# Second set contains no dublicates.
S2[1] = "ab";
S2[2] = "ac";
S2[3] = "ad";		
S2[4] = "ae";	 
S2[5] = "af";		
S2[6] = "ba";
S2[7] = "bc";	  
S2[8] = "ca";
S2[9] = "cb";	 
S2[10] = "cd";
		
S2[11] = "da";			
S2[12] = "dc";
S2[13] = "de";
S2[14] = "ea";
S2[15] = "ed";
S2[16] = "ef";		
S2[17] = "fa";
S2[18] = "fe";
S2[19] = "fg";
S2[20] = "gf";


# find_overlap finds the londest common suffix of sub_F that is a prefix of sub_B



find_overlap <- function(sub_F, sub_B, is_num)
{
	len = c(1:nchar(sub_F))
	
	
	# conditional statements below create two strings of the same length
	if (nchar(sub_F) > nchar(sub_B))
	{
		sF = substring(sub_F, nchar(sub_F) - nchar(sub_B) + 1, nchar(sub_F))
		sB = sub_B
	}
	
	else if (nchar(sub_B) > nchar(sub_F))
	{
		sF = sub_F
		sB = substring(sub_B, 1, nchar(sub_F))
	}
	
	else
	{
		sF = sub_F
		sB = sub_B
	}		
	
	# checks each prefix / suffix in decreaing order of length
	for ( i in len)
	{
		suffix = substring(sF, i, nchar(sF))
		prefix = substring(sB, 1, nchar(sB) - i + 1)
		
		if (suffix == prefix)
		{
			# method can return either the part of sub_B that isent a suffix of sub_F
			# or the size of the longest common prefix / suffix as numeric value
			if (is_num == FALSE)
			{
				return (substring(sub_B, nchar(suffix) + 1, nchar(sub_B)))
			}
			
			else
			{
				return (nchar(suffix))
			} 
		}
	}
	
	# if there is no common prefix / suffix
	if (is_num == F)
	{
		return (sub_B)
	}
	
	return (0)
	
}

# finds pairs of strings that are identical in a given set, dublicates were removed     
# maunally
find_dublicates <- function(string_set)
{
	len = c(1:length(string_set))
	
	M = matrix(len, nrow = length(string_set), ncol = length(string_set))
	
	for (i in len)
	{
		M[,i] = 0
		M[i,i] = 1

		for (j in i:length(string_set))
		{
			if (string_set[i] == string_set[j] & i != j)
			{
				M[j,i] = 1
				M[i,j] = 1
				print('MATCH: ')
				print(paste(i-1, j-1, sep =" <--> "))
				print(paste(string_set[i], string_set[j], sep =" <--> "))
				
			}
		}
	}
	
	return (M)
}


construct_super_string <- function(string_set)
{
	len = c(2:length(string_set) - 1)
	super_string = string_set[1]
	
	for (i in len)
	{
		add_on = find_overlap(string_set[i], string_set[i + 1], FALSE)
		super_string = paste(super_string, add_on, sep = "")
	}
	
	return (super_string)
}

# creates the matrix / graph from GSCS algorithm
construct_graph <- function(string_set)
{
	M = matrix(0, nrow = length(string_set), ncol = length(string_set))
	
	for (i in 1:nrow(M))
	{
		for (j in 1:ncol(M))
		{
			M[i,j] = find_overlap(string_set[i], string_set[j], T)	
		}
		
		M[i,i] = -1
	}
	
	return (M)
}

# merges a given pair of strings 
merge_strings <- function(sub_stringF, sub_stringB, isnum)
{
	if (nchar(sub_stringF) > nchar(sub_stringB))
	{
		sF = substring(sub_stringF, nchar(sub_stringF) - nchar(sub_stringB) + 1, nchar(sub_stringF))
		sB = sub_stringB
		sS = paste(sub_stringF, find_overlap(sF, sB, F), sep = "")
	}	
	
	else if (nchar(sub_stringB) > nchar(sub_stringF))
	{
		sF = sub_stringF
		sB = substring(sub_stringB, 1, nchar(sub_stringF))
		sS = paste(sF, find_overlap(sF, sB, F), substring(sub_stringB, nchar(sB) + 1, nchar(sub_stringB)), sep ="")
	}
	
	else
	{
		sF = sub_stringF
		sB = sub_stringB
		sS = paste(sF, find_overlap(sF, sB, F), sep = "")
	}
	
	return (sS)
}

# runs the greedy SCS for a given orderd input
greedySCS <- function(string_set)
{
	# shuffles input set
	SS = sample(string_set, length(string_set))
	
	while (length(SS) > 1)
	{
		G = construct_graph(SS)
		best_cost = -1
		
		# remove "#"s below to print the matix and chosen strings to merge at each stage 
		# of the algorithm
		
		#print(G)
		
		for (i in 1:length(SS))
		{
			for (j in 1:length(SS))
			{
				if (G[i,j] > best_cost)
				{
					best_edge = c(i,j)
					best_cost = G[i,j]
				}
			}
		}
		#print(best_edge)
		#print(best_cost)
		
		SS[best_edge[1]] = merge_strings(SS[best_edge[1]], SS[best_edge[2]], F)
		SS = SS[-best_edge[2]]
	}
			
	return(SS)
}

# runs the funtion above until a superstring of or below a given length is genterated
GSCS <- function(string_set, ref)
{
	Sstar = ""
	iter = 1
	
	for (i in 1:length(string_set))
	{
		Sstar = paste(Sstar, string_set[i], sep = "")
	}
	
	while (nchar(Sstar) > ref)
	{
		Sstar = greedySCS(string_set)
		iter = iter + 1
	}
	
	# remove "#" below to see the number of orders tested before a short enough 
	# superstring is produced
	
	#print(paste("# orders tested =", iter))
	return (Sstar) 
}


SuperString1 = GSCS(S1, 25)

SuperString2 = GSCS(S2, 21)

print(paste("SuperString1 = ",SuperString1))
print(paste("nchar = ", nchar(SuperString1)))

print(paste("SuperString2 = ",SuperString2))
print(paste("nchar = ", nchar(SuperString2)))














