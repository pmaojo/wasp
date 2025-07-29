import { useEffect, useState } from 'react'

export interface User {
  id: string
  name: string
}

export default function useUsers() {
  const [users, setUsers] = useState<User[]>([])

  useEffect(() => {
    fetch('/Users.json')
      .then((res) => res.json())
      .then(setUsers)
  }, [])

  return { users }
}
