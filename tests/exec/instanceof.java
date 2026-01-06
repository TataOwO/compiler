class A {
    A() {}
}

class B extends A {
    B() {}
}

class C extends B {
    C() { }
}

class Main {
    public static void main(String args[]) {
	B b = new B();

      	boolean ok = true;
	ok = ok && b instanceof A;
	ok = ok && b instanceof B;
	ok = ok && ! (b instanceof C);
	ok = ok && b instanceof Object;

	b = new C();
	ok = ok && b instanceof A;
	ok = ok && b instanceof B;
	ok = ok && b instanceof C;

	// null
	b = null;
	ok = ok && ! (b instanceof A);
	ok = ok && ! (b instanceof B);
	ok = ok && ! (b instanceof Object);
	ok = ok && ! (null instanceof String);

	// String
	String s = "" + 1;
	ok = ok && s instanceof String;
	ok = ok && s instanceof Object;
	ok = ok && "toto" instanceof String;
	ok = ok && "toto" instanceof Object;
        ok=ok&&""+1instanceof String;

	if (ok)
	    System.out.print("ok\n");
    }
}
