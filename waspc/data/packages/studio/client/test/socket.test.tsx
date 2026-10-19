import { renderHook } from '@testing-library/react'

// Mock socket.io-client before importing the module under test.
jest.mock('socket.io-client', () => {
  const socket = {
    on: jest.fn(),
    off: jest.fn(),
    connect: jest.fn(),
    disconnect: jest.fn(),
  }
  return { io: jest.fn(() => socket) }
})

import { useSocket, socket } from '../src/socket'

beforeEach(() => {
  jest.clearAllMocks()
})

test('socket connects on mount and disconnects on unmount', () => {
  const { unmount } = renderHook(() => useSocket())
  expect(socket.connect).toHaveBeenCalledTimes(1)
  unmount()
  expect(socket.disconnect).toHaveBeenCalledTimes(1)

  const { unmount: unmount2 } = renderHook(() => useSocket())
  expect(socket.connect).toHaveBeenCalledTimes(2)
  unmount2()
  expect(socket.disconnect).toHaveBeenCalledTimes(2)
})
