package com.autel.cloud.pile.base.domain.utils;

import lombok.extern.slf4j.Slf4j;
import org.apache.commons.lang.StringUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;

import javax.crypto.Cipher;
import javax.crypto.spec.IvParameterSpec;
import javax.crypto.spec.SecretKeySpec;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

/**
 * @author A22208
 */
@Slf4j
@Component
public class AesUtil {

    @Value("${application.aes.secretKeyStr}")
    private String secretKeyStr;

    @Value("${application.aes.iv}")
    private String iv;

    public String AES_ALGORITHM = "AES";

    public String encryptAes(byte[] content, String aes) {
        try {
            SecretKeySpec secretKeySpec = new SecretKeySpec(secretKeyStr.getBytes(), AES_ALGORITHM);
            Cipher cipher = Cipher.getInstance(aes);
            IvParameterSpec ivParameterSpec = new IvParameterSpec(iv.getBytes());
            cipher.init(Cipher.ENCRYPT_MODE, secretKeySpec, ivParameterSpec);
            return Base64.getEncoder().encodeToString(cipher.doFinal(content));
        } catch (Exception e) {
            log.error("加密失败, err is {}", e.getMessage());
            return StringUtils.EMPTY;
        }
    }

    public String decryptAes(byte[] content, String aes) {
        try {
            byte[] contentDecByBase64 = Base64.getDecoder().decode(content);
            SecretKeySpec secretKeySpec = new SecretKeySpec(secretKeyStr.getBytes(), AES_ALGORITHM);
            Cipher cipher = Cipher.getInstance(aes);
            IvParameterSpec ivParameterSpec = new IvParameterSpec(iv.getBytes());
            cipher.init(Cipher.DECRYPT_MODE, secretKeySpec, ivParameterSpec);
            return new String(cipher.doFinal(contentDecByBase64), StandardCharsets.UTF_8);
        } catch (Exception e) {
            log.error("解密失败, err is {}", e.getMessage());
            return StringUtils.EMPTY;
        }
    }

}
