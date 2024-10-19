/**
 * @author
 *
 */
public class GeneAnalysis
{
	/** Wrapper method. Returns the length of the longest 
	 * common subsequence
	 */
	public static int score(String gene1, String gene2)
	{
		int i = gene1.length()-1;
		int j = gene2.length()-1;
		char[] gene1Array = gene1.toCharArray();
		char[] gene2Array = gene2.toCharArray();
		return maxSubsequence(gene1Array, gene2Array, i, j);
		// Hint: Use toCharArray() to convert each string to a char array.
		 // call your recursive implementation here with
		// the genes as char arrays, starting at the end of each gene.
	}

	/** Implements longest common subsequence recursive search
The recursive case is defined as
					S(i-1, j)
S(i,j) = max {		S(i,j-1)
					S(i-1,j-1)
					S(i-1,j-1) +1 if gene1[i] = gene2[j]

NB  0<=i < gene1.length
    0<=j < gene2.length

You need to figure out the base case.
	 * */
private static int maxSubsequence(char[] gene1, char[] gene2, int i, int j){
	int count = 0;
	if(i < 0 || j < 0)
		return 0;
	if(gene1[i] != gene2[j]){
		count +=  Math.max(maxSubsequence(gene1, gene2, i-1, j),maxSubsequence(gene1, gene2, i, j-1));
	}
	if(gene1[i] == gene2[j]){
		count += 1 + maxSubsequence(gene1, gene2, i-1, j-1);
	}
	return count;
}
//	define a private recursive Class method 'score' that 
//	returns an integer the score.
//	The method should take four parameters- 
//	two char arrays and two integers (gene1,gene2,i,j)
//	i and j are the indices into gene1 and gene2 respectively.

}
// Use local variables to store a recursive result so that you  do not need to calculate it again.

// Do not use a loops.
