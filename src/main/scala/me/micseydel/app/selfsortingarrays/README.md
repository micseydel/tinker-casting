![](https://i.imgur.com/yLhBwUg.gif)

Based on [Classical Sorting Algorithms as a Model of Morphogenesis: self-sorting arrays reveal unexpected competencies in a minimal model of basal intelligence](https://arxiv.org/pdf/2401.05375)

MP4 @ https://imgur.com/a/4vwOQqK

Current status:
- NONLOCALInsertionSortCell was created as a "hello world"
  - This implementation passes the sorted list from the tail to the head
  - This was abandoned in favor of something more local - swapping only with neighbors
- (local) InsertionSortCell - partially working implementation
  - Problem: it stops halfway through the current example
  - Compromise: currently using a single thread to decrease nondeterminism
    - Without this limit, I have observed a race condition
