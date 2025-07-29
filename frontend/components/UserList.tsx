import useUsers from '../hooks/useUsers'

export default function UserList() {
  const { users } = useUsers()
  return (
    <ul>
      {users.map((u) => (
        <li key={u.id}>{u.name}</li>
      ))}
    </ul>
  )
}
