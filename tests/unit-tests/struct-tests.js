
var parent = types.makeStructureType('parent', 1, 0, false, false);
var child1 = types.makeStructureType('child1', 1, 1, parent, 'auto1');
var child2 = types.makeStructureType('child2', 2, 1, parent, 'auto2');

var subchild1 = types.makeStructureType('subchild1', 0, 0, child1, false);
var subchild2 = types.makeStructureType('subchild2', 2, 2, child2, 'auto2b');



// Constructors
var parentInstance = parent.constructor('a');
var child1Instance = child1.constructor('b', '1');
var child2Instance = child2.constructor('c', '2', '3');
var subchild1Instance = subchild1.constructor('d', '4');
var subchild2Instance = subchild2.constructor('e', '5', '6', 7, 8);

sys.print(sys.inspect(parentInstance) + '\n');
sys.print(sys.inspect(child1Instance) + '\n');
sys.print(sys.inspect(child2Instance) + '\n');
sys.print(sys.inspect(subchild1Instance) + '\n');
sys.print(sys.inspect(subchild2Instance) + '\n');


// Predicates
assert.ok( parent.predicate(parentInstance) );
assert.ok( parent.predicate(child1Instance) );
assert.ok( parent.predicate(child2Instance) );
assert.ok( parent.predicate(subchild1Instance) );
assert.ok( parent.predicate(subchild2Instance) );

assert.ok( !child1.predicate(parentInstance) );
assert.ok( child1.predicate(child1Instance) );
assert.ok( !child1.predicate(child2Instance) );
assert.ok( child1.predicate(subchild1Instance) );
assert.ok( !child1.predicate(subchild2Instance) );

assert.ok( !child2.predicate(parentInstance) );
assert.ok( !child2.predicate(child1Instance) );
assert.ok( child2.predicate(child2Instance) );
assert.ok( !child2.predicate(subchild1Instance) );
assert.ok( child2.predicate(subchild2Instance) );

assert.ok( !subchild1.predicate(parentInstance) );
assert.ok( !subchild1.predicate(child1Instance) );
assert.ok( !subchild1.predicate(child2Instance) );
assert.ok( subchild1.predicate(subchild1Instance) );
assert.ok( !subchild1.predicate(subchild2Instance) );

assert.ok( !subchild2.predicate(parentInstance) );
assert.ok( !subchild2.predicate(child1Instance) );
assert.ok( !subchild2.predicate(child2Instance) );
assert.ok( !subchild2.predicate(subchild1Instance) );
assert.ok( subchild2.predicate(subchild2Instance) );


// Accessors
assert.deepEqual(parent.accessor(parentInstance, 0), 'a');
assert.deepEqual(parent.accessor(child1Instance, 0), 'b');
assert.deepEqual(parent.accessor(child2Instance, 0), 'c');
assert.deepEqual(parent.accessor(subchild1Instance, 0), 'd');
assert.deepEqual(parent.accessor(subchild2Instance, 0), 'e');

assert.deepEqual(child1.accessor(child1Instance, 0), '1');
assert.deepEqual(child1.accessor(child1Instance, 1), 'auto1');
assert.deepEqual(child1.accessor(subchild1Instance, 0), '4');
assert.deepEqual(child1.accessor(subchild1Instance, 1), 'auto1');

assert.deepEqual(child2.accessor(child2Instance, 0), '2');
assert.deepEqual(child2.accessor(child2Instance, 1), '3');
assert.deepEqual(child2.accessor(child2Instance, 2), 'auto2');
assert.deepEqual(child2.accessor(subchild2Instance, 0), '5');
assert.deepEqual(child2.accessor(subchild2Instance, 1), '6');
assert.deepEqual(child2.accessor(subchild2Instance, 2), 'auto2');

assert.deepEqual(subchild2.accessor(subchild2Instance, 0), 7);
assert.deepEqual(subchild2.accessor(subchild2Instance, 1), 8);
assert.deepEqual(subchild2.accessor(subchild2Instance, 2), 'auto2b');
assert.deepEqual(subchild2.accessor(subchild2Instance, 3), 'auto2b');

sys.print('All tests passed!!\n');
