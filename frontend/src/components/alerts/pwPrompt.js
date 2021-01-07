import swal from '@sweetalert/with-react'
import './alert.css'

export default async () => {
  return await swal({
    text: "Enter Password To Decrypt Private Key",
    title: "Sign Transaction",
    content: {
      element: "input",
      attributes: {
        placeholder: "Type your password",
        type: "password",
      },
    },
  })
};
