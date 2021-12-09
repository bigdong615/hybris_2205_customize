package com.bl.esp.service.impl;

import com.bl.esp.constants.BlespintegrationConstants;
import com.bl.esp.service.BlFTPService;
import com.bl.logging.BlLogger;
import com.jcraft.jsch.Channel;
import com.jcraft.jsch.ChannelSftp;
import com.jcraft.jsch.JSch;
import com.jcraft.jsch.Session;
import java.io.File;
import java.io.FileInputStream;
import org.apache.log4j.Level;
import org.apache.log4j.Logger;

/**
 * This class created to send file to FTP
 * @author Manikandan
 */
public class DefaultBlESPFTPService implements BlFTPService {

  private static final Logger LOG = Logger.getLogger(DefaultBlESPFTPService.class);

  /**
   *  {@inheritDoc}  
   */
  @Override
  public void sendFileTOFTP() {
    Session session = null;
    Channel channel = null;
    ChannelSftp channelSftp = null;
    try {
      JSch jsch = new JSch();
      session = jsch.getSession(BlespintegrationConstants.SFTPUSER, BlespintegrationConstants.SFTPHOST,
          BlespintegrationConstants.SFTPPORT);
      session.setPassword(BlespintegrationConstants.SFTPPASS);
      java.util.Properties config = new java.util.Properties();
      config.put("StrictHostKeyChecking", "no");
      session.setConfig(config);
      session.connect();
      channel = session.openChannel("sftp");
      channel.connect();
      channelSftp = (ChannelSftp) channel;
      channelSftp.cd(BlespintegrationConstants.SFTPWORKINGDIR);
      File f = new File("");
      channelSftp.put(new FileInputStream(f), f.getName());
    } catch (final Exception ex) {
      BlLogger.logMessage(LOG , Level.ERROR , "Error while performing sendFileTOFTP:-" , ex);
    } finally {
      channelSftp.exit();
      channel.disconnect();
      session.disconnect();
    }
  }
}
