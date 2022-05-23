with RDP; use RDP;
with Ada.Unchecked_Deallocation;
package body Queues is

    QueueIsEmpty: exception;


    
    procedure Free is new Ada.Unchecked_Deallocation (Node, NodePtr);

    function Size(q: Queue) return Natural is (q.Size);
    function IsEmpty(q: Queue) return Boolean is (q.First = null and q.Last = null and q.Size = 0);
    procedure Push(q: in out Queue; element: Header)
    is
        n : NodePtr; 
    begin
        n := new Node'(element, null);
        q.Last.Next := n;
        q.Last := n;    
        q.Size := q.Size + 1;
    end Push;
    
    procedure Pop(q: in out Queue; element: out Header) is
        tmp: Header;
    begin
        if q.First /= null then
            tmp := q.First.Element;
            
            if q.First.Next /= null then
                q.First := q.First.Next;
            else
                -- only a single element
                q.First := null;
                q.Last := null;
            end if;
            --deallocate the now unreferenced node
            Free(q.First);

            q.Size := q.Size - 1;
        else
            raise QueueIsEmpty;
        end if;

        element := tmp;
    end Pop;

end Queues;