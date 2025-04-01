import ast
import graphviz

class Brick(int):
    """
    A subclass of int representing a brick, with custom arithmetic and comparison behavior.
    """

    @classmethod
    def from_string(cls, value: str) -> 'Brick':
        """Creates a Brick instance from a string value."""
        return cls(int(value))

    def __add__(self, other):
        """Adds two bricks or a brick and an int."""
        if isinstance(other, Brick):
            return Brick(super().__add__(other))
        elif isinstance(other, int):
            return Brick(super().__add__(other))
        else:
            return NotImplemented

    def __radd__(self, other):
        """Handles addition when the brick is on the right side."""
        return self.__add__(other)

    def __mul__(self, other):
        """Multiplies two bricks or a brick and an int."""
        if isinstance(other, Brick):
            return Brick(super().__mul__(other))
        elif isinstance(other, int):
            return Brick(super().__mul__(other))
        else:
            return NotImplemented

    def __rmul__(self, other):
        """Handles multiplication when the brick is on the right side."""
        return self.__mul__(other)

    def __sub__(self, other):
        """Subtracts two bricks or a brick and an int."""
        if isinstance(other, Brick):
            return Brick(super().__sub__(other))
        elif isinstance(other, int):
            return Brick(super().__sub__(other))
        else:
            return NotImplemented

    def __rsub__(self, other):
        """Handles subtraction when the brick is on the right side."""
        if isinstance(other, int):
            return Brick(other - int(self))
        else:
            return NotImplemented

    def __gt__(self, other):
        """Greater than comparison."""
        if isinstance(other, Brick):
            return super().__gt__(other)
        elif isinstance(other, int):
            return super().__gt__(other)
        else:
            return NotImplemented

    def __lt__(self, other):
        """Less than comparison."""
        if isinstance(other, Brick):
            return super().__lt__(other)
        elif isinstance(other, int):
            return super().__lt__(other)
        else:
            return NotImplemented

    def __ge__(self, other):
        """Greater than or equal to comparison."""
        if isinstance(other, Brick):
            return super().__ge__(other)
        elif isinstance(other, int):
            return super().__ge__(other)
        else:
            return NotImplemented

    def __le__(self, other):
        """Less than or equal to comparison."""
        if isinstance(other, Brick):
            return super().__le__(other)
        elif isinstance(other, int):
            return super().__le__(other)
        else:
            return NotImplemented

    def __eq__(self, other):
        """Equality comparison."""
        if isinstance(other, Brick):
            return super().__eq__(other)
        elif isinstance(other, int):
            return super().__eq__(other)
        else:
            return NotImplemented


class Arithmetic:
    """
    A class that represents composite arithmetic operations on bricks.
    This allows for operations like (brick1 + brick2) * brick3.
    """
    
    def __init__(self, value, operation=None, left=None, right=None):
        """Initialize with either a Brick, int, or another Arithmetic."""
        if isinstance(value, (Brick, int, Arithmetic)):
            self.value = value
            self.operation = operation
            self.left = left
            self.right = right
        else:
            raise TypeError("Arithmetic can only be initialized with Brick, int, or Arithmetic")

    def __add__(self, other):
        """Adds two arithmetic expressions or an arithmetic expression and a brick/int."""
        if isinstance(other, (Brick, int, Arithmetic)):
            return Arithmetic(self.value + other, "+", self, other)
        return NotImplemented

    def __radd__(self, other):
        """Handles addition when the arithmetic expression is on the right side."""
        return Arithmetic(other + self.value, "+", other, self)

    def __mul__(self, other):
        """Multiplies two arithmetic expressions or an arithmetic expression and a brick/int."""
        if isinstance(other, (Brick, int, Arithmetic)):
            return Arithmetic(self.value * other, "x", self, other)
        return NotImplemented

    def __rmul__(self, other):
        """Handles multiplication when the arithmetic expression is on the right side."""
        return Arithmetic(other * self.value, "x", other, self)

    def __sub__(self, other):
        """Subtracts two arithmetic expressions or an arithmetic expression and a brick/int."""
        if isinstance(other, (Brick, int, Arithmetic)):
            return Arithmetic(self.value - other, "-", self, other)
        return NotImplemented

    def __rsub__(self, other):
        """Handles subtraction when the arithmetic expression is on the right side."""
        return Arithmetic(other - self.value, "-", other, self)

    def __int__(self):
        """Converts the arithmetic expression to an integer."""
        return int(self.value)

    def __str__(self):
        """String representation of the arithmetic expression."""
        if self.operation is None:
            return str(self.value)
        return f"({self.left} {self.operation} {self.right})"

    def __repr__(self):
        """Repr representation of the arithmetic expression."""
        if self.operation is None:
            return f"Arithmetic({self.value})"
        return f"Arithmetic({self.value}, '{self.operation}', {self.left}, {self.right})"


class ArithmeticTree:
    """
    A class that generates SVG visualizations of arithmetic operation trees using Graphviz.
    """
    
    def __init__(self, arithmetic):
        """Initialize with an Arithmetic instance."""
        if not isinstance(arithmetic, Arithmetic):
            raise TypeError("ArithmeticTree can only be initialized with an Arithmetic instance")
        self.arithmetic = arithmetic
        # Parse the arithmetic expression into an AST
        self.ast_tree = self._parse_arithmetic(arithmetic)

    def _parse_arithmetic(self, node):
        """Convert an Arithmetic instance into an AST node, using the AST module."""
        if isinstance(node, (Brick, int)):
            return ast.Num(n=node)
        elif isinstance(node, Arithmetic):
            if node.operation is None:
                return self._parse_arithmetic(node.value)
            
            # Create a BinOp node based on the operation
            op_map = {
                '+': ast.Add(),
                '-': ast.Sub(),
                'x': ast.Mult(),
                '*': ast.Mult()
            }
            
            # Parse left and right nodes recursively
            left = self._parse_arithmetic(node.left)
            right = self._parse_arithmetic(node.right)
            
            if left is None or right is None:
                return None
                
            return ast.BinOp(
                left=left,
                op=op_map[node.operation],
                right=right
            )
        return None

    def to_svg(self):
        """Generate the SVG representation of the arithmetic tree using Graphviz."""
        # Create a new directed graph
        dot = graphviz.Digraph(comment='Arithmetic Expression Tree')
        dot.attr(rankdir='TB')  # Top to bottom layout
        
        # Set graph attributes for better visualization - black and white only
        dot.attr('node', shape='box', style='filled', fillcolor='white', fontname='Arial', color='black')
        dot.attr('edge', fontname='Arial', color='black')
        dot.attr('graph', 
                bgcolor='white',
                margin='0.5',
                size='8,6')

        # Function to add nodes and edges recursively
        def add_node(node, parent_id=None, edge_label=None):
            if isinstance(node, ast.Num):
                node_id = f"num_{id(node)}"
                dot.node(node_id, str(node.n))
                if parent_id:
                    dot.edge(parent_id, node_id, edge_label or '')
                return node_id
            elif isinstance(node, ast.BinOp):
                # Get the operation symbol
                op_symbol = {
                    ast.Add: '+',
                    ast.Sub: '-',
                    ast.Mult: '×'
                }.get(type(node.op), '?')
                
                node_id = f"op_{id(node)}"
                dot.node(node_id, op_symbol)
                if parent_id:
                    dot.edge(parent_id, node_id, edge_label or '')
                add_node(node.left, node_id, '')
                add_node(node.right, node_id, '')
                return node_id

        # Add nodes starting from the root
        root_id = add_node(self.ast_tree)
        
        # Add a title to the graph
        dot.attr(label=f'Arithmetic Expression Tree\nValue: {self.arithmetic}')
        
        # Generate SVG with explicit rendering
        return dot.pipe(format='svg').decode('utf-8')

    def save(self, filename="arithmetic_tree.svg"):
        """Save the SVG tree to a file."""
        svg_content = self.to_svg()
        with open(filename, "w") as f:
            f.write(svg_content)
        return filename


if __name__ == "__main__":
    # Example Usage
    brick1 = Brick(5)
    brick2 = Brick(10)
    brick3 = Brick(30)
    
    # Testing new decorated methods

    print(f"Creating brick from string: {Brick.from_string('15')}")
    
    # Original examples
    print(brick1 + brick2)
    print(brick1 * 3)
    print((brick2 - brick1) * brick3)
    print(brick1 > 3)
    print(brick1 < 10)
    print(brick1 >= 5)
    print(brick1 <= brick2)
    print(brick1 == 5)
    print(3 + brick1)  # radd example
    print(3 - brick1)  # rsub example
    print(3 * brick1)  # rmul example

    # Testing Arithmetic
    print("\nTesting Arithmetic:")
    # Complex expression: (((5 + 10) × 2) - 5) × ((30 - 5) + 15)
    
    # Left side: ((5 + 10) × 2) - 5
    sum_left = Arithmetic(brick1 + brick2, "+", brick1, brick2)  # (5 + 10)
    mult_by_two = Arithmetic(sum_left.value * 2, "x", sum_left, 2)  # (5 + 10) × 2
    subtract_five = Arithmetic(mult_by_two.value - 5, "-", mult_by_two, 5)  # ((5 + 10) × 2) - 5
    
    # Right side: (30 - 5) + 15
    diff_right = Arithmetic(brick3 - brick1, "-", brick3, brick1)  # (30 - 5)
    add_fifteen = Arithmetic(diff_right.value + 15, "+", diff_right, 15)  # (30 - 5) + 15
    
    # Final expression
    final_expr = Arithmetic(subtract_five.value * add_fifteen.value, "x", subtract_five, add_fifteen)
    
    print(f"Complex expression: {final_expr}")
    print(final_expr.value)

    # Testing ArithmeticTree with a complex expression
    print("\nGenerating arithmetic tree visualization...")
    tree = ArithmeticTree(final_expr)
    filename = tree.save()
    print(f"Tree visualization saved to {filename}")