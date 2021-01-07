import swal from '@sweetalert/with-react'

export default async () => {
  console.log('in sw')
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
