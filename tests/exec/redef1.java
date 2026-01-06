class A { 
    int a;
    A() { }
    void set_a(int a) { this.a = a; }
}

class B extends A {
    B() { }
    void set_a(int a) { this.a = a * 2; }
}

class Main {
    public static void main(String args[]) {
	A a = new A();
	B b = new B();
	a.set_a(1);
	b.set_a(1);
	if (a.a == 1 && b.a == 2)
	    System.out.print("ok\n");
    }
}
