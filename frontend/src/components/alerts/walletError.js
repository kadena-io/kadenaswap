import swal from '@sweetalert/with-react'
import './alert.css'

export default async () => {
  return await swal({
    text: "You cancelled the transaction or do not have the wallet app open",
    title: "Wallet Signing Failure"
  })
};
