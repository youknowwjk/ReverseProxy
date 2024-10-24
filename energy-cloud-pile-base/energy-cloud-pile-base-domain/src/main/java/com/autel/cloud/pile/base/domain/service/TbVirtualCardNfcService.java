package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbVirtualCardNfcEntity;
import com.autel.cloud.pile.base.vo.TbVirtualCardNfcVO;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.web.multipart.MultipartFile;

/**
 * @author A22587
 */
public interface TbVirtualCardNfcService extends IService<TbVirtualCardNfcEntity> {


    TbVirtualCardNfcVO getNfcCardNumber(Integer nfcType, Long userId);

    Boolean validNFCCardNo(String cardNumber, String userId);

    Result<String> uploadNFCCertificateModuleXls(MultipartFile file, String path);

    Result<TbVirtualCardNfcVO> getNfcCardInfoByNumber(String cardNumber);
}
