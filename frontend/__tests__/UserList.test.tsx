import { render, screen } from '@testing-library/react'
import UserList from '../components/UserList'

jest.mock('../hooks/useUsers', () => () => ({ users: [{ id: '1', name: 'Test' }] }))

test('renders users', () => {
  render(<UserList />)
  expect(screen.getByText('Test')).toBeInTheDocument()
})
