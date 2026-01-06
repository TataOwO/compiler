class A { int x; A() { } }

class Main {
    public static void main(String args[]) {
	A a = new A();
	boolean b = true;

	int i1 = (int)-3;
	b = b && i1 == -3;

	i1 = (i1)+4;
	b = b && i1 == 1;

	i1 = (int)i1;
	b = b && i1 == 1;

	i1 = (int)(i1+1);
	b = b && i1 == 2;

	i1 = (int)a.x;
	b = b && i1 == 0;

	i1 = (int)(a.x+2);
	b = b && i1 == 2;

	i1 = -(int)(int)-i1;
	b = b && i1 == 2;

	i1 = (int)(i1);
	b = b && i1 == 2;

	if (! (boolean)!b)
	    System.out.print("ok\n");
    }
}
