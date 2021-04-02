import swal from '@sweetalert/with-react'
import './alert.css'

export default async () => {
  return await swal({
    text: "If you forgot your password you can always reset it using your private key in the account section",
    title: "Incorrect Password"
  })
};
