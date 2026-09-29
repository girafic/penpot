--- The moment (ms) of the animation of the board a thread is on that
--- the thread is about (Penpot Motion); NULL for a thread that is not
--- about a moment of an animation.

ALTER TABLE comment_thread
  ADD COLUMN animation_time integer NULL;
