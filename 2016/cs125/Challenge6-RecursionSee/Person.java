/**
 * @author
 *
 */
public class Person
{
private final String name;
private final int age;
private final char gender;
private final Person child1; //left child
private final Person child2; //right child

public Person(String name, int age, char gender, Person c1, Person c2)
{
    this.name=name;
    this.age=age;
    this.gender=gender;
    this.child1 = c1;
    this.child2 = c2;
}

public String toString()
{
    return name+"*"+age+"*"+gender;
}

public String getName() 
{
	return name;
}

public int getAge() 
{
	return age;
}

public char getGender() 
{
	return gender;
}

public boolean equals(Person p)
{
	return (this.name.equals(p.name)) &&
			(this.age==p.age) &&
			(this.gender==p.gender);
}


public void print() 
{
   System.out.println(this);
   if(child1 != null)
       child1.print();
   if(child2 != null)
       child2.print();
}

public int count() // total person count including this object
{
	int count = 1;
	if(child1 != null) count += child1.count();
	if(child2 != null) count += child2.count();
	return count;
}

public int countGrandChildren() // but not greatGrandChildren
{
	return countChildren(2);
}

private int countChildren(int gen){
	int count = 0;
	if(gen==0){
		return 1;
	}
	if(gen>0){
		if(child1 != null) count += child1.countChildren(gen-1);
		if(child2 != null) count += child2.countChildren(gen-1);
	}
	return count;
}

public int countMaxGenerations()
{
	int countLeft = 1;
	int countRight = 1;
	if(child1 != null) countLeft += child1.countMaxGenerations();
	if(child2 != null) countRight += child2.countMaxGenerations();
	return Math.max(countLeft, countRight);
}

public int countGender(char gen)
{
	int count = 0;
	if(this.gender == gen) count++;
	if(child1 != null) count += child1.countGender(gen);
	if(child2 != null) count += child2.countGender(gen);
	return count;
}

public Person search(String name, int maxGeneration)
{
	Person n = null;
	if(this.name.equals(name))
		return this;
	if(maxGeneration <= 0)
		return null;
	if(child1 != null) n = child1.search(name, maxGeneration-1);
	if(n != null) return n;
	if(child2 != null) n = child2.search(name, maxGeneration-1);
	return n;
}

} // end of class