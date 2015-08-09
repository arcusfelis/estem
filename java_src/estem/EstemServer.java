package estem;

import java.util.logging.Logger;
import java.util.List;
import java.util.ArrayList;
import java.nio.charset.Charset;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpNode;
import com.ericsson.otp.stdlib.OtpContinueException;
import com.ericsson.otp.stdlib.OtpGenServer;
import com.ericsson.otp.stdlib.OtpStopException;
import org.getopt.stempel.Stemmer;

/**
 * @author Fernando Benavides <elbrujohalcon@inaka.net> Main OtpGenServer for
 *         Estem
 */
public class EstemServer extends OtpGenServer {
	private static final Logger	jlog	= Logger.getLogger(EstemServer.class
												.getName());

    private Stemmer stemmer;
	/**
	 * @param host
	 *            OtpNode where to create the OtpGenServer
	 */
	public EstemServer(OtpNode host) {
		super(host, "estem_server");
        stemmer = new Stemmer();
	}

	@Override
	protected OtpErlangObject handleCall(OtpErlangObject cmd,
			OtpErlangTuple from) throws OtpStopException, OtpContinueException,
			OtpErlangException {
		OtpErlangTuple cmdTuple = (OtpErlangTuple) cmd;
		OtpErlangAtom cmdName = (OtpErlangAtom) cmdTuple.elementAt(0);
        String res, token;
		if (cmdName.atomValue().equals("stem")) { // {stem, [Str]}
            OtpErlangList stringList = (OtpErlangList) cmdTuple.elementAt(1);

            List<OtpErlangObject> array_list = new ArrayList<OtpErlangObject>();
            for (int i = 0; i < stringList.arity(); i++) {

              OtpErlangBinary stringBin = (OtpErlangBinary) stringList.elementAt(i);
              
              byte[] bytes = stringBin.binaryValue();
              token = new String(bytes, Charset.forName("UTF-8"));

              res = stemmer.stem(token, false);
              
              if (res != null)
                {
                    byte[] bytes_out = res.getBytes(Charset.forName("UTF-8"));
                    OtpErlangBinary bin_out = new OtpErlangBinary(bytes_out);
                    array_list.add(bin_out);
                }
            }
            OtpErlangObject[] array = array_list.toArray(new OtpErlangObject[ array_list.size() ]);
            OtpErlangList list = new OtpErlangList(array);
            return list;
		} else
		if (cmdName.atomValue().equals("pid")) { // {pid}
			return super.getSelf();
		} else
			return new OtpErlangTuple(new OtpErlangObject[] {
					new OtpErlangAtom("error"),
					new OtpErlangAtom("unknown command") });
	}

	@Override
	protected void handleCast(OtpErlangObject cmd) throws OtpStopException,
			OtpErlangException {
		OtpErlangTuple cmdTuple = (OtpErlangTuple) cmd;
		String cmdName = ((OtpErlangAtom) cmdTuple.elementAt(0)).atomValue();
		if (cmdName.equals("stop")) { // {stop}
			jlog.info("Stopping");
			throw new OtpStopException();
		}
	}

	@Override
	protected void handleInfo(OtpErlangObject cmd) throws OtpStopException {
	}
}
