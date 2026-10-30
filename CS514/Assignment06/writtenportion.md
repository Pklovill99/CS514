Written Questions

1. What is the difference between a typing judgment and a typing derivation?
A typing judgment is just saying a term has a type in a context like Gamma |- t : T. A typing derivation is the whole proof tree that shows why that is true using the rules.

2. Why is a typing context necessary?
Free variables need types. The context keeps track of what variables are in scope and what type they have. Without it we couldnt type check the body of a lambda or any open term. Looking from the front of the list also handles shadowing.

3. Why must both branches of if have the same type?
The whole if expression has to have one type that doesnt depend on the run time value of the condition. If the branches had different types the expression wouldnt have a single well defined type.

4. Why must T-App require an arrow type in function position?
You can only apply something that is a function. The rule checks that the function part has a type like T1 -> T2. This lets us check the argument against T1 and know the result has type T2. Applying a non function is rejected.

5. How does a typing context differ from the runtime environment used by a closure based evaluator?
A typing context maps names to types and is only used while type checking. A runtime environment maps names to actual values and is used while the program is running. They look similar both are name bindings but they store different things and are used at different times.

6. Why does the recursive structure of a type checker resemble a proof tree?
Each recursive call of the type checker corresponds to a premise of an inference rule. The way the checker combines the results of those calls is the same way a proof tree combines the conclusions of its sub derivations. So the call tree of a successful type check is basically the typing derivation.