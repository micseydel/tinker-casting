starting...
# t=1

```mermaid
sequenceDiagram
    autonumber
    participant env as env
    participant C0 as 0 (val=28)<br/>INDEX=0<br/>1 ->
box rgb(31, 236, 117)
    participant C1 as 1 (val=34)<br/>INDEX=1<br/><- 0 | 2 ->
    participant C2 as 2 (val=6)<br/>INDEX=2<br/><- 1 | 3 ->
end
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 2 | 4 ->
    participant C4 as 4 (val=7)<br/>INDEX=4<br/><- 3 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=5<br/><- 4 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=6<br/><- 5 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=7<br/><- 6 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=8<br/><- 7 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 8
    env--)C0: DoSort
    C0-)C1: DoSort
    C0-)C1: DoSort
activate C1
    C1-)C2: BeginSwap(newLeft=0)
    C2-)C1: CompleteSwap(newRight=3)
deactivate C1
    C2--)C3: NotifyOfSwap(left(2)=1)
    C2-)C1: DoSort
Note left of C2: <-
Note left of C3: NEW LEFT Some(1)
    C1--)C0: NotifyOfSwap(right(1)=2)
Note right of C1: ->
Note over C1,C2: SWAPPED
Note right of C0: NEW RIGHT Some(2)
```
# t=2

```mermaid
sequenceDiagram
    autonumber
box rgb(107, 81, 78)
    participant C0 as 0 (val=28)<br/>INDEX=0<br/>2 ->
    participant C2 as 2 (val=6)<br/>INDEX=1<br/><- 0 | 1 ->
end
box rgb(13, 163, 168)
    participant C1 as 1 (val=34)<br/>INDEX=2<br/><- 2 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 1 | 4 ->
end
    participant C4 as 4 (val=7)<br/>INDEX=4<br/><- 3 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=5<br/><- 4 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=6<br/><- 5 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=7<br/><- 6 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=8<br/><- 7 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 8
activate C0
    C0-)C2: BeginSwap(newLeft=x)
activate C1
    C1-)C3: BeginSwap(newLeft=2)
    C2-)C1: DoSort
    C2-)C0: CompleteSwap(newRight=1)
deactivate C0
    C0--)C1: NotifyOfSwap(left(1)=0)
    C2-)C0: DoSort
Note left of C2: <-
    C3-)C1: CompleteSwap(newRight=4)
deactivate C1
    C3--)C4: NotifyOfSwap(left(3)=1)
    C3-)C1: DoSort
Note left of C3: <-
Note left of C4: NEW LEFT Some(1)
    C0--)C3: NotifyOfSwap(left(1)=0)
Note left of C1: NEW LEFT Some(0)
    C1--)C0: NotifyOfSwap(right(2)=3)
Note right of C1: ->
Note over C1,C3: SWAPPED
    C0-)C1: DoSort
Note right of C0: ->
Note over C0,C2: SWAPPED
Note right of C0: NEW RIGHT Some(3)
    C3-)C1: DoSort
Note left of C3: NEW LEFT Some(0)
```
# t=3

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>0 ->
box rgb(45, 156, 38)
    participant C0 as 0 (val=28)<br/>INDEX=1<br/><- 2 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=2<br/><- 0 | 1 ->
end
box rgb(196, 63, 77)
    participant C1 as 1 (val=34)<br/>INDEX=3<br/><- 3 | 4 ->
    participant C4 as 4 (val=7)<br/>INDEX=4<br/><- 1 | 5 ->
end
    participant C5 as 5 (val=89)<br/>INDEX=5<br/><- 4 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=6<br/><- 5 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=7<br/><- 6 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=8<br/><- 7 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 8
activate C0
    C0-)C3: BeginSwap(newLeft=2)
activate C1
    C1-)C4: BeginSwap(newLeft=3)
    C2-)C0: DoSort
    C3-)C1: DoSort
    C3-)C0: CompleteSwap(newRight=1)
deactivate C0
    C0--)C1: NotifyOfSwap(left(2)=0)
    C3--)C2: NotifyOfSwap(right(1)=3)
    C3-)C0: DoSort
Note left of C3: <-
    C4-)C1: CompleteSwap(newRight=5)
deactivate C1
    C4--)C5: NotifyOfSwap(left(4)=1)
    C4-)C1: DoSort
Note left of C4: <-
Note left of C5: NEW LEFT Some(1)
    C0--)C2: NotifyOfSwap(right(1)=3)
    C0-)C1: DoSort
Note right of C0: ->
Note over C0,C3: SWAPPED
    C0--)C4: NotifyOfSwap(left(2)=0)
Note left of C1: NEW LEFT Some(0)
    C1--)C0: NotifyOfSwap(right(3)=4)
    C1-)C5: DoSort
Note right of C1: ->
Note over C1,C4: SWAPPED
    C2-)C3: DoSort
Note right of C2: NEW RIGHT Some(3)
    C2-)C3: DoSort
Note right of C2: NEW RIGHT Some(3)
    C4-)C1: DoSort
Note left of C4: NEW LEFT Some(0)
Note right of C0: NEW RIGHT Some(4)
```
# t=4

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>3 ->
    participant C3 as 3 (val=20)<br/>INDEX=1<br/><- 2 | 0 ->
box rgb(122, 93, 55)
    participant C0 as 0 (val=28)<br/>INDEX=2<br/><- 3 | 4 ->
    participant C4 as 4 (val=7)<br/>INDEX=3<br/><- 0 | 1 ->
end
    participant C1 as 1 (val=34)<br/>INDEX=4<br/><- 4 | 5 ->
box rgb(66, 47, 42)
    participant C5 as 5 (val=89)<br/>INDEX=5<br/><- 1 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=6<br/><- 5 | 7 ->
end
    participant C7 as 7 (val=18)<br/>INDEX=7<br/><- 6 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=8<br/><- 7 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 8
activate C0
    C0-)C4: BeginSwap(newLeft=3)
    C1-)C5: DoSort
    C2-)C3: DoSort
    C3-)C0: DoSort
    C4-)C1: DoSort
    C4-)C0: CompleteSwap(newRight=1)
deactivate C0
    C0--)C1: NotifyOfSwap(left(3)=0)
    C4--)C3: NotifyOfSwap(right(2)=4)
    C4-)C0: DoSort
Note left of C4: <-
activate C5
    C5-)C6: BeginSwap(newLeft=1)
    C6-)C5: CompleteSwap(newRight=7)
deactivate C5
    C6--)C7: NotifyOfSwap(left(6)=5)
    C6-)C5: DoSort
Note left of C6: <-
Note left of C7: NEW LEFT Some(5)
    C0--)C3: NotifyOfSwap(right(2)=4)
    C0-)C1: DoSort
Note right of C0: ->
Note over C0,C4: SWAPPED
    C1-)C5: DoSort
Note left of C1: NEW LEFT Some(0)
Note right of C3: NEW RIGHT Some(4)
Note right of C3: NEW RIGHT Some(4)
    C5--)C1: NotifyOfSwap(right(5)=6)
Note right of C5: ->
Note over C5,C6: SWAPPED
    C1-)C6: DoSort
Note right of C1: NEW RIGHT Some(6)
```
# t=5

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>3 ->
box rgb(183, 19, 144)
    participant C3 as 3 (val=20)<br/>INDEX=1<br/><- 2 | 4 ->
    participant C4 as 4 (val=7)<br/>INDEX=2<br/><- 3 | 0 ->
end
    participant C0 as 0 (val=28)<br/>INDEX=3<br/><- 4 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=4<br/><- 0 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=5<br/><- 1 | 5 ->
box rgb(102, 87, 9)
    participant C5 as 5 (val=89)<br/>INDEX=6<br/><- 6 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=7<br/><- 5 | 8 ->
end
    participant C8 as 8 (val=29)<br/>INDEX=8<br/><- 7 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 8
    C0-)C1: DoSort
    C1-)C6: DoSort
    C2-)C3: DoSort
activate C3
    C3-)C4: BeginSwap(newLeft=2)
    C4-)C0: DoSort
    C4-)C3: CompleteSwap(newRight=0)
deactivate C3
    C3--)C0: NotifyOfSwap(left(2)=3)
    C4--)C2: NotifyOfSwap(right(1)=4)
    C4-)C3: DoSort
Note left of C4: <-
activate C5
    C5-)C7: BeginSwap(newLeft=6)
    C6-)C5: DoSort
    C7-)C5: CompleteSwap(newRight=8)
deactivate C5
    C7--)C8: NotifyOfSwap(left(7)=5)
    C7-)C5: DoSort
Note left of C7: <-
Note left of C8: NEW LEFT Some(5)
    C0-)C1: DoSort
Note left of C0: NEW LEFT Some(3)
    C3--)C2: NotifyOfSwap(right(1)=4)
    C3-)C0: DoSort
Note right of C3: ->
Note over C3,C4: SWAPPED
    C2-)C4: DoSort
Note right of C2: NEW RIGHT Some(4)
    C2-)C4: DoSort
Note right of C2: NEW RIGHT Some(4)
    C5--)C6: NotifyOfSwap(right(6)=7)
Note right of C5: ->
Note over C5,C7: SWAPPED
Note right of C6: NEW RIGHT Some(7)
```
# t=6

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=2<br/><- 4 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=3<br/><- 3 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=4<br/><- 0 | 6 ->
box rgb(254, 91, 236)
    participant C6 as 6 (val=34)<br/>INDEX=5<br/><- 1 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=6<br/><- 6 | 5 ->
end
box rgb(4, 170, 174)
    participant C5 as 5 (val=89)<br/>INDEX=7<br/><- 7 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=8<br/><- 5 | 9 ->
end
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 8
    C0-)C1: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C3: DoSort
activate C5
    C5-)C8: BeginSwap(newLeft=7)
activate C6
    C6-)C7: BeginSwap(newLeft=1)
    C7-)C5: DoSort
    C7-)C6: CompleteSwap(newRight=5)
deactivate C6
    C6--)C5: NotifyOfSwap(left(6)=6)
    C7--)C1: NotifyOfSwap(right(5)=7)
    C7-)C6: DoSort
Note left of C7: <-
    C8-)C5: CompleteSwap(newRight=9)
deactivate C5
    C8--)C9: NotifyOfSwap(left(8)=5)
    C8-)C5: DoSort
Note left of C8: <-
Note left of C9: NEW LEFT Some(5)
    C6--)C8: NotifyOfSwap(left(6)=6)
Note left of C5: NEW LEFT Some(6)
    C5--)C6: NotifyOfSwap(right(7)=8)
Note right of C5: ->
Note over C5,C8: SWAPPED
    C6--)C1: NotifyOfSwap(right(5)=7)
    C6-)C5: DoSort
Note right of C6: ->
Note over C6,C7: SWAPPED
Note right of C6: NEW RIGHT Some(8)
Note right of C1: NEW RIGHT Some(7)
Note right of C1: NEW RIGHT Some(7)
    C8-)C5: DoSort
Note left of C8: NEW LEFT Some(6)
```
# t=7

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=2<br/><- 4 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=3<br/><- 3 | 1 ->
box rgb(234, 117, 19)
    participant C1 as 1 (val=34)<br/>INDEX=4<br/><- 0 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=5<br/><- 1 | 6 ->
end
box rgb(48, 249, 231)
    participant C6 as 6 (val=34)<br/>INDEX=6<br/><- 7 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=7<br/><- 6 | 5 ->
end
box rgb(79, 9, 99)
    participant C5 as 5 (val=89)<br/>INDEX=8<br/><- 8 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=9<br/><- 5
end
    C0-)C1: DoSort
activate C1
    C1-)C7: BeginSwap(newLeft=0)
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C3: DoSort
activate C5
    C5-)C9: BeginSwap(newLeft=8)
activate C6
    C6-)C8: BeginSwap(newLeft=7)
    C7-)C6: DoSort
    C7-)C1: CompleteSwap(newRight=6)
deactivate C1
    C1--)C6: NotifyOfSwap(left(5)=1)
    C7--)C0: NotifyOfSwap(right(4)=7)
    C7-)C1: DoSort
Note left of C7: <-
    C8-)C5: DoSort
    C8-)C6: CompleteSwap(newRight=5)
deactivate C6
    C6--)C5: NotifyOfSwap(left(7)=6)
    C8--)C7: NotifyOfSwap(right(6)=8)
    C8-)C6: DoSort
Note left of C8: <-
    C9-)C5: CompleteSwap(newRight=x)
deactivate C5
    C9-)C5: DoSort
Note left of C9: <-
Note right of C0: NEW RIGHT Some(7)
    C1--)C8: NotifyOfSwap(left(5)=1)
Note left of C6: NEW LEFT Some(1)
    C6--)C1: NotifyOfSwap(right(6)=8)
    C6-)C5: DoSort
Note right of C6: ->
Note over C6,C8: SWAPPED
    C1--)C0: NotifyOfSwap(right(4)=7)
    C1-)C6: DoSort
Note right of C1: ->
Note over C1,C7: SWAPPED
Note right of C1: NEW RIGHT Some(8)
    C6--)C9: NotifyOfSwap(left(7)=6)
Note left of C5: NEW LEFT Some(6)
    C5--)C6: NotifyOfSwap(right(8)=9)
Note right of C5: ->
Note over C5,C9: SWAPPED
    C8--)C1: NotifyOfSwap(right(6)=8)
    C7-)C1: DoSort
    C8-)C6: DoSort
Note left of C8: NEW LEFT Some(1)
Note right of C0: NEW RIGHT Some(7)
    C6-)C9: DoSort
Note right of C6: NEW RIGHT Some(9)
    C9-)C5: DoSort
Note left of C9: NEW LEFT Some(6)
Note right of C1: NEW RIGHT Some(8)
```
# t=8

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=2<br/><- 4 | 0 ->
box rgb(96, 107, 161)
    participant C0 as 0 (val=28)<br/>INDEX=3<br/><- 3 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=4<br/><- 0 | 1 ->
end
box rgb(21, 104, 73)
    participant C1 as 1 (val=34)<br/>INDEX=5<br/><- 7 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=6<br/><- 1 | 6 ->
end
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 8 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
activate C0
    C0-)C7: BeginSwap(newLeft=3)
activate C1
    C1-)C8: BeginSwap(newLeft=7)
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C3: DoSort
    C6-)C9: DoSort
    C7-)C1: DoSort
    C7-)C0: CompleteSwap(newRight=1)
deactivate C0
    C0--)C1: NotifyOfSwap(left(4)=0)
    C7--)C3: NotifyOfSwap(right(3)=7)
    C7-)C0: DoSort
Note left of C7: <-
    C8-)C6: DoSort
    C8-)C1: CompleteSwap(newRight=6)
deactivate C1
    C1--)C6: NotifyOfSwap(left(6)=1)
    C8--)C7: NotifyOfSwap(right(5)=8)
    C8-)C1: DoSort
Note left of C8: <-
    C9-)C5: DoSort
    C0--)C3: NotifyOfSwap(right(3)=7)
    C0-)C1: DoSort
Note right of C0: ->
Note over C0,C7: SWAPPED
Note right of C3: NEW RIGHT Some(7)
Note right of C3: NEW RIGHT Some(7)
    C0--)C8: NotifyOfSwap(left(4)=0)
Note left of C1: NEW LEFT Some(0)
    C1--)C0: NotifyOfSwap(right(5)=8)
    C1-)C6: DoSort
Note right of C1: ->
Note over C1,C8: SWAPPED
    C6-)C9: DoSort
Note left of C6: NEW LEFT Some(1)
    C8--)C0: NotifyOfSwap(right(5)=8)
    C7-)C0: DoSort
    C8-)C1: DoSort
Note left of C8: NEW LEFT Some(0)
    C0-)C8: DoSort
Note right of C0: NEW RIGHT Some(8)
    C0-)C8: DoSort
```
# t=9

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 3 ->
box rgb(48, 193, 67)
    participant C3 as 3 (val=20)<br/>INDEX=2<br/><- 4 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=3<br/><- 3 | 0 ->
end
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 7 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
activate C3
    C3-)C7: BeginSwap(newLeft=4)
    C4-)C3: DoSort
    C6-)C9: DoSort
    C7-)C0: DoSort
    C7-)C3: CompleteSwap(newRight=0)
deactivate C3
    C3--)C0: NotifyOfSwap(left(3)=3)
    C7--)C4: NotifyOfSwap(right(2)=7)
    C7-)C3: DoSort
Note left of C7: <-
    C8-)C1: DoSort
    C9-)C5: DoSort
    C3--)C4: NotifyOfSwap(right(2)=7)
    C3-)C0: DoSort
Note right of C3: ->
Note over C3,C7: SWAPPED
    C0-)C8: DoSort
Note left of C0: NEW LEFT Some(3)
    C4-)C7: DoSort
Note right of C4: NEW RIGHT Some(7)
    C4-)C7: DoSort
Note right of C4: NEW RIGHT Some(7)
```

- ⏰(9) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=10

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(10) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=11

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(11) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=12

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(12) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=13

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(13) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=14

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(14) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=15

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(15) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=16

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(16) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=17

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(17) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=18

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(18) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=19

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(19) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=20

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(20) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=21

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(21) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
# t=22

```mermaid
sequenceDiagram
    autonumber
    participant C2 as 2 (val=6)<br/>INDEX=0<br/>4 ->
    participant C4 as 4 (val=7)<br/>INDEX=1<br/><- 2 | 7 ->
    participant C7 as 7 (val=18)<br/>INDEX=2<br/><- 4 | 3 ->
    participant C3 as 3 (val=20)<br/>INDEX=3<br/><- 7 | 0 ->
    participant C0 as 0 (val=28)<br/>INDEX=4<br/><- 3 | 8 ->
    participant C8 as 8 (val=29)<br/>INDEX=5<br/><- 0 | 1 ->
    participant C1 as 1 (val=34)<br/>INDEX=6<br/><- 8 | 6 ->
    participant C6 as 6 (val=34)<br/>INDEX=7<br/><- 1 | 9 ->
    participant C9 as 9 (val=51)<br/>INDEX=8<br/><- 6 | 5 ->
    participant C5 as 5 (val=89)<br/>INDEX=9<br/><- 9
    C0-)C8: DoSort
    C1-)C6: DoSort
    C2-)C4: DoSort
    C3-)C0: DoSort
    C4-)C7: DoSort
    C6-)C9: DoSort
    C7-)C3: DoSort
    C8-)C1: DoSort
    C9-)C5: DoSort
```

- ⏰(22) \[2, 4, 7, 3, 0, 8, 1, 6, 9, 5]
