-- | Typed mailbox abstraction for actors.
--
-- Provides a clean API over STM's TQueue.
-- Mailboxes are unbounded queues (backpressure deferred for MVP).
module ExoMonad.Actor.Mailbox
  ( -- * Mailbox Type
    Mailbox,

    -- * Creation
    newMailboxIO,

    -- * Operations
    send,
    receive,
  )
where

import Control.Concurrent.STM (atomically)
import Control.Concurrent.STM.TQueue
  ( TQueue,
    newTQueueIO,
    readTQueue,
    writeTQueue,
  )

-- | A mailbox is an unbounded queue for receiving messages.
--
-- Wraps STM's 'TQueue' with a cleaner API.
newtype Mailbox msg = Mailbox (TQueue msg)

-- | Create a new empty mailbox.
newMailboxIO :: IO (Mailbox msg)
newMailboxIO = Mailbox <$> newTQueueIO

-- | Send a message to a mailbox.
--
-- Non-blocking, always succeeds (unbounded queue).
send :: Mailbox msg -> msg -> IO ()
send (Mailbox q) msg = atomically $ writeTQueue q msg

-- | Receive a message from a mailbox.
--
-- Blocks until a message is available.
receive :: Mailbox msg -> IO msg
receive (Mailbox q) = atomically $ readTQueue q
