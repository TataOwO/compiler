class A {
    int a;
    B b;
    A() {}
}

class B extends A {
    int b;
    B() {}
}

class C extends B {
    int c;
    C() { }
}

class Main {
    public static void main(String args[]) {
	A a = new A();
	B b = new B();
	C c = new C();
	int x;
	x = ((A)a).a;
	x = ((A)b).a;
	x = ((A)c).a;
	Object o = (Object)b;
	o = (A)null;
	o = (Object)"tutu";
	String s = (String)"toto";
	boolean ok = (B)a.b == null;
	ok = ok && ((A)b).b == null;

	// types primitifs
	ok = ok && (int)1 == 1 && (int)1 == 1;
	ok = ok && (boolean)true;

	// multi cast
	ok = ok && ((C)(B)(A)(Object)(A)(B)(C)new C()).c == 0;

	if (ok)
	    System.out.print("ok\n");
    }
}
