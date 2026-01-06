class A { 
    void init() { System.out.print("ok\n"); }
}

class B extends A { 
    B() { init(); } 
}

class Main {
    public static void main(String args[]) {
	B b = new B();
    }
}
