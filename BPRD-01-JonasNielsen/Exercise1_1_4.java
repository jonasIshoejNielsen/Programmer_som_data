import java.util.Map;

public class Exercise1_1_4 {
    static abstract class Expr {
        public abstract  int eval(Map<String, Integer> env);
        public abstract  Expr simplify();
    }
    
    static class CstI extends Expr {
        protected final int i;
        public CstI(int i) {this.i = i;}
        @Override
        public String toString() { return i+""; }

        @Override
        public int eval(Map<String, Integer> env) {
            return i;
        }

        @Override
        public Expr simplify() {
            return this;
        }
    }
    static class Var extends Expr {
        protected final String v;
        public Var(String v) {this.v = v;}
        @Override
        public String toString() { return v; }

        @Override
        public int eval(Map<String, Integer> env) {
            return env.get(v);
        }

        @Override
        public Expr simplify() {
            return this;
        }
    }
    static abstract class Binop extends Expr {
    }
    static class Add extends Binop {
        protected final Expr ae1;
        protected final Expr ae2;
        public Add(Expr ae1, Expr ae2) {this.ae1 = ae1; this.ae2 = ae2;}
        @Override
        public String toString() { return "("+ae1 + " + " + ae2+")"; }

        @Override
        public int eval(Map<String, Integer> env) {
            return ae1.eval(env)+ae2.eval(env);
        }

        @Override
        public Expr simplify() {
            if(ae1 instanceof CstI) {
                int val = ((CstI) ae1).i;
                if(val == 0) return ae2.simplify();
            }
            if(ae2 instanceof CstI) {
                int val = ((CstI) ae2).i;
                if(val == 0) return ae1.simplify();
            }
            return new Add(ae1.simplify(), ae2.simplify());
        }
    }
    static class Mul extends Binop {
        protected final Expr ae1;
        protected final Expr ae2;
        public Mul(Expr ae1, Expr ae2) {this.ae1 = ae1; this.ae2 = ae2;}
        @Override
        public String toString() { return "("+ae1 + " * " + ae2+")"; }

        @Override
        public int eval(Map<String, Integer> env) {
            return ae1.eval(env)*ae2.eval(env);
        }

        @Override
        public Expr simplify() {
            if(ae1 instanceof CstI) {
                int val = ((CstI) ae1).i;
                if(val == 1) return ae2.simplify();
                if(val == 0) return new CstI(0);
            }
            if(ae2 instanceof CstI) {
                int val = ((CstI) ae2).i;
                if(val == 1) return ae2.simplify();
                if(val == 0) return new CstI(0);
            }
            return new Mul(ae1.simplify(), ae2.simplify());
        }
    }
    static class Sub extends Binop {
        protected final Expr ae1;
        protected final Expr ae2;
        public Sub(Expr ae1, Expr ae2) {this.ae1 = ae1; this.ae2 = ae2;}
        @Override
        public String toString() { return "("+ae1 + " - " + ae2+")"; }
        @Override
        public int eval(Map<String, Integer> env) {
            return ae1.eval(env)-ae2.eval(env);
        }

        @Override
        public Expr simplify() {
            if(ae2 instanceof CstI) {
                int val = ((CstI) ae2).i;
                if(val == 0) return ae1.simplify();
            }
            return new Sub(ae1.simplify(), ae2.simplify());
        }
    }

    public static void main(String[] args) {
        Expr e = new Add(new CstI(17), new Var("z"));
        Expr e1 = new Mul(new CstI(17), e);
        Expr e2 = new Sub(e, e1);
        Expr e3 = new Add(e2, new Var("z"));
        System.out.println(e.toString());
        System.out.println(e1.toString());
        System.out.println(e2.toString());
        System.out.println(e3.toString());

        Expr e4 = new Sub(new Add(new CstI(17), new CstI(0)), new CstI(0));
        System.out.println(e4.simplify());

    }
}
