with RDP; use RDP;
-- generic
--     type E is private;
package Queues is
    type Queue is private;
    function Size(Q: Queue) return Natural;
    function IsEmpty(Q: Queue) return Boolean;
    procedure Pop(Q:in out Queue; element: out Header) 
    with 
        Pre => not IsEmpty (Q);
        --Post => Size(Q) = Size(Q'Old) - 1 and Size(Q) >= 0;
    procedure Push(Q: in out Queue; element: in Header) ;
       -- with Post => Size(Q) = Size(Q'Old) + 1 and not IsEmpty (Q);

private
    type Node;
    type NodePtr is access Node;
    type Node is record
        Element: Header;
        Next: NodePtr;
    end record;

    type Queue is record
        First: NodePtr;
        Last: NodePtr;
        Size: Natural;
    end record;    
end Queues;