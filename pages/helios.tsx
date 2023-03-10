import type { NextPage } from 'next'
import Link from 'next/link';
import WalletConnect from '../components/WalletConnect'
import { useStoreState } from "../utils/store"
import { useState, useEffect } from 'react'
import initLucid from '../utils/lucid'
import { Lucid, Constr, SpendingValidator, Data, Assets } from 'lucid-cardano'
import { Blockfrost } from 'lucid-cardano/types/src/core/wasm_modules/cardano_multiplatform_lib_web/cardano_multiplatform_lib'
import { getAssets } from "../utils/cardano";

const Helios: NextPage = () => {
  const walletStore = useStoreState((state: any) => state.wallet)
  const [nftList, setNftList] = useState([])
  const [lucid, setLucid] = useState<Lucid>()
  const [script, setScript] = useState<SpendingValidator>()
  const [scriptAddress, setScriptAddress] = useState("")
  const [myTxHash, setMyTxHash] = useState("")
  const [show, setShow] = useState();

  useEffect(() => {
    if (lucid) {
      totalAmountOfAdaInScript();

    } else {
      initLucid(walletStore.name).then((Lucid: Lucid) => { setLucid(Lucid) })
    }
  }, [lucid])

  const multiSigScript: SpendingValidator = {
    type: "PlutusV2",
    script:
      "590a58590a55010000323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232323232222533535301f00322353010003223500222222222222232533553335734607e0020642a666ae68c0f80040b40f44cc0780f0ccc0600400440144c8c8c8c8c8cc06ccc08c104ccc074054058028cc06ccc08d241264e6577206b657920636f756e74206973206e6f742067726561746572207468616e207a65726f003301a00248000cc06ccc08d2412b4e657720726571756972656420636f756e74206973206e6f742067726561746572207468616e207a65726f003301a00148000cc06ccc08d241304e657720726571756972656420636f756e74206973206e6f742067726561746572207468616e206b657920636f756e7400333573466e240040080e00cccc08d2411b4475706c6963617465206b65797320696e206e657720646174756d003301c33020303c03d333049222533500203a221533533353304e221222533500103c22153353300500200404313335300704900500400104202400200313300500100313303e002330050013303e00200300303833020303c03d0033500303d3301e303a03b0013500103c53335533553353500122350022222222222223333500d203e203e203e23335530470483302d22533500221003100103e2350012253355333573466e3cd4008130d40101304ccd5cd19b873500204d3500404d0460410401304200304000d21350012235001222235008223500222222222222233355304c04e2235002222253353501822350062232335005233500425333573466e3c00800400c14081448cd4010814494ccd5cd19b8f002001003050153350032153350022133500223350022335002233500223304000200120542335002205423304000200122205422233500420542225333573466e1c01800c54ccd5cd19b8700500213303f00400105505504e15335001204e052133050006005100504900a15335738921024c6600161303603c22135002222253350051002221303e044130364901124d697373696e6720446174756d204861736800213032001215335350112222222222223305222533500103d22135002225333573466e3c00804c4c10c0044c01800c00884c0cc0044c0dd2410f446174756d206e6f7420666f756e6400301f0123042375402422400226048921035054350022333573466e2400800406c080894cd4008400406488ccd5cd19b8700200101e019222333573466e20cc018c08808cccd54c08408c894cd4ccd54c090094cc02888ccc0200a0008004c01809cc01c0080144cc08400800440040780040080640788cc004894cd4008070400405888ccd5cd19b8f00200101b01622333355002330042233350050220010023500302122337000029001000a40006600244460066004002400244246600200600444a66a00202c266ae700080488c94ccd5cd18109813000899191919091980080180118069aba135744006a666ae68c08cc0a00044c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c8c848cccccccccccc00406005805004804003803002401c01400c008c0a0d5d09aba200233302175c40026ae84004d5d100119980f81010009aba10013574400466050eb8d5d08009aba20035333573460666070002264646464642466002008004a666ae68c0dcc0f00044c8c8c848cc00400c008c06cd5d09aba20023301b75a6ae84004c0ec0040d8dd51aba135744006a666ae68c0d4c0e80044c8c8c848cc00400c008c064d5d09aba20023301975a6ae84004c0e40040d0dd51aba1001303700103237546ae84004d5d100119980c00dbad357420026ae88008cc084060d5d08009aba200233301475c0266ae84004d5d10011998093ae011357420026ae88008cc06c034d5d08009aba20023301900a357420026ae88008cc05c020d5d080098138008111baa35742002604a0020406ea80048c94ccd5cd181080080a0a999ab9a302000100f01f30243754002446464a666ae68c08c0044488800454ccd5cd18110008990911180180218021aba13025002153335734604200222444004040604a0026ea80048c94ccd5cd180f18118008991919091980080180118029aba13574400460186ae84004c088004074dd50009192999ab9a301d30220011323232323232323232321233330010090070030023300c75c6ae84d5d10022999ab9a3027001132122230020043574260520042a666ae68c0980044c84888c004010dd71aba13029002153335734604a0022244400604860520026ea8d5d08009aba200233300975c0106ae84004d5d1001a999ab9a301f3024001132323212330010030023300700c357426ae88008c030d5d0800981180080f1baa3574200260420020386ea800488c8c94ccd5cd180f0008980798021aba13022002153335734603e00201a03a60440026ea8004cc005d73ad222330212233335573e002402646464646602e2466002006004600c6ae88014c01cc090008c01cc090004d5d080100b9bab0012323253335734603a00226424444600800a60086ae84c07800854ccd5cd180e00089909111180100298029aba1301e002153335734603600226424444600200a600e6ae84c07800854ccd5cd180d0008990911118018029bae35742603c004032603c0026ea80048c8c94ccd5cd19b874803000444888888800c54ccd5cd19b874802800444888888801054ccd5cd19b87480200044c8c848888888cc004024020dd69aba135744603c0066eb8d5d0980e8010a999ab9a301c001132321222222233002009008375c6ae84d5d1180f0019bae35742603a0042a666ae68c06c0044c8c848888888cc018024020dd71aba135744603c00660086ae84c07400854ccd5cd180d00089909111111180380418021aba1301d002153335734603200226424444444600a01060086ae84c074008060c074004dd5000919192999ab9a301900113232323232122333001006004003375a6ae84d5d10011bad357420026ae88008dd69aba1001301c0021533357346030002264244600400660086ae84c07000805cc070004dd5000919192999ab9a30180011321223001003375c6ae84c06c00854ccd5cd180b80089909118010019bae35742603600402c60360026ea80048c94ccd5cd180a980d000899191909198008018011bad357426ae88008c010d5d0800980c80080a1baa001232533357346028603200226eb8d5d0980c0008099baa0011001100c21223002003232533357346020602a002264646424660020060046600aeb8d5d09aba2002375a6ae84004c05000403cdd5000911980a911999aab9f001200723300830053574200460066ae8800802cdd60008800880311091198008020019299ab9c00116300e22112225335001100222133005002333553007009005004001300d221122253350011350030082213335005009300400233355300700800500400112001220022200149012665457065637465642065786163746c79206f6e6520636f6e74696e75696e67206f75747075740049011b4e6f7420656e6f7567682076616c6964207369676e61747572657300153357389201035054310016370e90001b8748008dc3a40086e1d20065573caae748c8c00400488cc00cc008008005",
  };

  const redeemer =
    Data.to(new Constr(1, []))
    
  const multiSigLockFunds = async () => {
    if (lucid) {

      const multiSigScriptAddress: any = lucid.utils.validatorToAddress(
        multiSigScript,
      );
    
      //datum
      const myWallet = await lucid?.wallet.address();
      const { paymentCredential } = lucid?.utils.getAddressDetails(myWallet)!;
      const iRequiredCount = BigInt(1);
      const iKeys = [paymentCredential!.hash, '8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121',
        'ac4b6cbdde85cff6b95254df0d92ba3d6a559f92c297cd48488bb41b'];
    
        // const iKeys = [new Constr (0, [paymentCredential!.hash]), 
        //         new Constr (0, ["8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121"])]
      const inputDatum = new Constr(0, [
        iRequiredCount,
        iKeys
      ]);
      
      const tx = await lucid
        .newTx()
        .payToContract(
          multiSigScriptAddress,
          {
            inline: Data.to(inputDatum),
          },
          {
            lovelace: BigInt(10000000),
          }
        )
        .complete();

      const signedTx = await tx.sign().complete();
      const txHash = await signedTx.submit();
      setMyTxHash(txHash);
    }
  }

  const multiSigClose = async () => {
    if (lucid) {

      const multiSigScriptAddress: any = lucid.utils.validatorToAddress(
        multiSigScript,
      );

      const myWallet = await lucid?.wallet.address();
      const { paymentCredential } = lucid?.utils.getAddressDetails(myWallet)!;
      const iRequiredCount = BigInt(1);
      const iKeys = [paymentCredential!.hash, '8fd2af318fe6fd7a8b2f56861b7dda312411281616b902953abf7121',
        'ac4b6cbdde85cff6b95254df0d92ba3d6a559f92c297cd48488bb41b'];
    
      const inputDatum = new Constr(0, [
        iRequiredCount,
        iKeys
      ]);

      const inputDatumString = Data.to(inputDatum);
      const inputDatumHash = lucid.utils.datumToHash(inputDatumString);
      const utxo = (await lucid.utxosAt(multiSigScriptAddress)).
                                filter(u => lucid.utils.datumToHash(u.datum as string) === inputDatumHash);
      if (!utxo) throw new Error("Spending script utxo not found");

      const tx = await lucid
        .newTx()
        .collectFrom(utxo, redeemer)
        .attachSpendingValidator(multiSigScript)
        .addSignerKey(paymentCredential?.hash!)
        .complete();

      const signedTx = await tx.sign().complete();
      await signedTx.submit();
    }
  }

  const hasEligableNFT = async () => {
    if (lucid) {

      const myWallet = await lucid?.wallet.address();
      const hasNft = (await lucid.utxosAt(myWallet))
                        .filter((utxo) => 
                          Object.keys(utxo.assets).some((asset) => 
                            asset.startsWith('d07ecd3fa808b6757cea92b3da14c5ccb8587f0e9e98e5c5d16bb2d9'))).length > 0;
      console.log(hasNft);
    }
  }

  const totalAmountOfAdaInScript = async () => {
    if (lucid) {
      const multiSigScriptAddress: any = lucid.utils.validatorToAddress(
        multiSigScript,
      );
      const myWallet = await lucid?.wallet.address();

      const totalAmountofADAScript = await fetch(
        `https://cardano-preprod.blockfrost.io/api/v0/addresses/${multiSigScriptAddress}`,
        {
          headers: {
            // Your Blockfrost API key
            project_id: process.env.NEXT_PUBLIC_BLOCKFROST!,
            'Content-Type': 'application/json'
          }
        }
      ).then(res => res.json());

      const totalAmountofADAMyWallet = await fetch(
        `https://cardano-preprod.blockfrost.io/api/v0/addresses/${myWallet}`,
        {
          headers: {
            // Your Blockfrost API key
            project_id: process.env.NEXT_PUBLIC_BLOCKFROST!,
            'Content-Type': 'application/json'
          }
        }
      ).then(res => res.json());

      const nfts = await fetch(
        `https://cardano-preprod.blockfrost.io/api/v0/assets/policy/${'d07ecd3fa808b6757cea92b3da14c5ccb8587f0e9e98e5c5d16bb2d9'}`,
        {
          headers: {
            // Your Blockfrost API key
            project_id: process.env.NEXT_PUBLIC_BLOCKFROST!,
            'Content-Type': 'application/json'
          }
        }
      ).then(res => res.json());

      const myWalletBalance: string[] = Object.values(totalAmountofADAMyWallet['amount'][0]);
      let balance2 = (parseInt(myWalletBalance[1]) / 1000000).toFixed(2);
      console.log('MY WALLET BALANCE IN LOVELACE: ' + myWalletBalance);
      console.log("TOTAL BALANCE OF SCRIPT IN ADA:" + balance2.toString());

      console.log(nfts[0]['quantity']);
      console.log(totalAmountofADAScript);

      const totalScriptBalance: string[] = Object.values(totalAmountofADAScript['amount'][0]);
      let balance = (parseInt(totalScriptBalance[1]) / 1000000).toFixed(2);
      console.log("TOTAL BALANCE OF SCRIPT IN LOVELACE:" + totalScriptBalance);
      console.log("TOTAL BALANCE OF SCRIPT IN ADA:" + balance.toString());
      let lbl = document.getElementById('balanceElem');
      lbl!.innerText= balance.toString() + " ADA";

      const numberOfUtxos = (await lucid.utxosAt(multiSigScriptAddress)).length;
      let lblVotes = document.getElementById('votesElem');
      lblVotes!.innerText= numberOfUtxos.toString();
    }
  }



  return (

    <div className="px-10">
      <div>
        <Link href="/">
          <button className="btn btn-primary m-5 justify-center" >HOME</button>
        </Link>
      </div>

      <div className="text-center pt-8">
        <div className="">
          <WalletConnect />
        </div>
      </div>

      <div className="flex items-center justify-center my-10">
        {/* <button className="btn btn-primary m-5" onClick={() => { multiSigClose() }} >Sign</button>
        <button className="btn btn-primary m-5" onClick={() => { multiSigClose() }} >Close</button> */}
        {/* <button className="btn btn-primary m-5" onClick={() => { lockUtxo() }} >Deposit</button> */}
        {/* <button className="btn btn-secondary m-5" onClick={() => { redeemUtxo() }}>Unlock</button> */}
        {/* <button className="btn btn-secondary m-5" onClick={() => { mintNft() }}>Mint NFT</button> */}
      </div>


      <div className="grid mt-8 grid-cols-3 gap-4 stat">
          <div className="card w-96 bg-base-100 shadow-xl">
            <figure className="px-10 pt-10 ">
              <img src="https://placeimg.com/640/480/tech" alt="Shoes" className="rounded-xl" />
            </figure>
            <div className="card-body items-center text-center">
              <h2 className="card-title">Proposal 1</h2>
              <p>Fund new governance tools written in haskell</p>
              <div className="card-actions">
              <button className="btn btn-primary" onClick={() => { hasEligableNFT() }}>Vote</button>
                {/* <div id="proposalActions" >
                <button className="btn btn-primary m-5" onClick={() => { multiSigLockFunds() }} >Lock Funds</button>
                 <button className="btn btn-primary m-5" onClick={() => { multiSigClose() }} >Redeem</button>
                </div> */}
              </div>
            </div>

            <div className="stat">
              <div className="stat-title">Balance</div>
              <div id= "balanceElem" className="stat-value"></div>
              <div className="stat-actions">
                <button className="btn btn-sm btn-success"onClick={() => { multiSigLockFunds() }}>Add funds</button>
                <button className="btn btn-sm" onClick={() => { multiSigClose() }}>Withdraw</button>
              </div>
            </div>
            
            <div className="stat">
            <div className="stat-title">Number of Votes</div>
              <div id = "votesElem" className="stat-value"></div>
            </div>
          </div>

          <div className="card w-96 bg-base-100 shadow-xl">
            <figure className="px-10 pt-10 ">
              <img src="https://placeimg.com/640/480/tech/sepia" alt="Shoes" className="rounded-xl" />
            </figure>
            <div className="card-body items-center text-center">
              <h2 className="card-title">Proposal 2</h2>
              <p>Support improved integration to Lucid framework</p>
              <div className="card-actions">
                <button className="btn btn-primary">Vote</button>
              </div>
            </div>

            <div className="stat">
              <div className="stat-title">Balance</div>
              <div className="stat-value">9,000 Ada</div>
              <div className="stat-actions">
                <button className="btn btn-sm btn-success">Add funds</button>
                <button className="btn btn-sm">Withdraw</button>
              </div>
            </div>
            
            <div className="stat">
            <div className="stat-title">Number of Votes</div>
              <div className="stat-value">23</div>
            </div>
          </div>

          <div className="card w-96 bg-base-100 shadow-xl">
            <figure className="px-10 pt-10 ">
              <img src="https://placeimg.com/640/480/arch/grayscale" alt="Shoes" className="rounded-xl" />
            </figure>
            <div className="card-body items-center text-center">
              <h2 className="card-title">Proposal 3</h2>
              <p>Fund tools created to power better oracles for off-chain data</p>
              <div className="card-actions">
                <button className="btn btn-primary">Vote</button>
              </div>
            </div>

            <div className="stat">
              <div className="stat-title">Balance</div>
              <div className="stat-value">9,000 Ada</div>
              <div className="stat-actions">
                <button className="btn btn-sm btn-success">Add funds</button>
                <button className="btn btn-sm">Withdraw</button>
              </div>
            </div>
            
            <div className="stat">
            <div className="stat-title">Number of Votes</div>
              <div className="stat-value">23</div>
            </div>
          </div>

          <div className="card w-96 bg-base-100 shadow-xl">
            <figure className="px-10 pt-10 ">
              <img src="https://placeimg.com/640/480/tech/grayscale" alt="Shoes" className="rounded-xl" />
            </figure>
            <div className="card-body items-center text-center">
              <h2 className="card-title">Proposal 4</h2>
              <p>Fund new opensource multisig contracts</p>
              <div className="card-actions">
                <button className="btn btn-primary">Vote</button>
              </div>
            </div>

            <div className="stat">
              <div className="stat-title">Balance</div>
              <div className="stat-value">9,000 Ada</div>
              <div className="stat-actions">
                <button className="btn btn-sm btn-success">Add funds</button>
                <button className="btn btn-sm">Withdraw</button>
              </div>
            </div>
            
            <div className="stat">
            <div className="stat-title">Number of Votes</div>
              <div className="stat-value">23</div>
            </div>
          </div>

        </div>

      
      <div className="mx-40 my-10 place-items-center">

        {/* <button className="btn btn-primary m-5" onClick={() => { hasEligableNFT() }} >hasNFT</button>
        <button className="btn btn-primary m-5" onClick={() => { totalAmountOfAdaInScript() }} >Total amount</button>
        <label id="lblEmp">N/A</label>
        <div></div> */}
      </div>
    </div>
  )
}

export default Helios