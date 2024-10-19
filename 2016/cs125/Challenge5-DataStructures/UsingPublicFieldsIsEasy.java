//@author
/**
 * Complete the class method 'analyze' that takes a SimplePublicPair object as an argument
 * and returns a new SimplePublicTriple object.
 * The SimplePublicTriple needs to set up as follows:
 * x = the minimum value of 'a' and 'b'
 * y = the maximum value of 'a' and 'b'
 * description:a*b=M 
 *   where a,b, and M are replaced with the numerical values of a, b and the multiplication of a and b.
 * Your code will create a SimplePublicTriple, initializes the three fields and return a reference to the SimplePublicTriple object.
 *
 */
public class UsingPublicFieldsIsEasy {
	
	public static SimplePublicTriple analyze(SimplePublicPair in) {
		SimplePublicTriple result = new SimplePublicTriple();
		if(in.b > in.a){
			result.x = in.a;
			result.y = in.b;
		}
		else{
			result.y = in.a;
			result.x = in.b;
		}
		result.description = in.a +"*"+ in.b + "=" + (in.a * in.b);
		return result;
	}
}
