package com.autel.cloud.pile.base.domain.service.impl;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.TbVirtualCardNfcRepository;
import com.autel.cloud.pile.base.domain.service.TbVirtualCardNfcService;
import com.autel.cloud.pile.base.infrastructure.mapper.TbVirtualCardNfcMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbVirtualCardNfcEntity;
import com.autel.cloud.pile.base.vo.TbVirtualCardNfcVO;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;

/**
 * @author A22587
 */
@Service
@Slf4j
public class TbVirtualCardNfcServiceImpl extends ServiceImpl<TbVirtualCardNfcMapper, TbVirtualCardNfcEntity> implements TbVirtualCardNfcService {

    @Resource
    private TbVirtualCardNfcRepository tbVirtualCardNfcRepository;


    @Override
    public TbVirtualCardNfcVO getNfcCardNumber(Integer nfcType, Long userId) {
        return tbVirtualCardNfcRepository.getNfcCardNumber(nfcType, userId);
    }

    @Override
    public Boolean validNFCCardNo(String cardNumber, String userId) {
        return tbVirtualCardNfcRepository.validNFCCardNo(cardNumber, userId);
    }

    @Override
    public Result<String> uploadNFCCertificateModuleXls(MultipartFile file, String path) {
        return Result.ofSucceed(tbVirtualCardNfcRepository.uploadNFCCertificateModuleXls(file, path));
    }

    @Override
    public Result<TbVirtualCardNfcVO> getNfcCardInfoByNumber(String cardNumber) {
        return tbVirtualCardNfcRepository.getNfcCardInfoByNumber(cardNumber);
    }

}
