package com.autel.cloud.pile.base.domain.repository.impl;

import cn.hutool.core.util.CharsetUtil;
import com.alibaba.excel.util.FileUtils;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.exception.MessageCodeException;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.domain.repository.TbVirtualCardNfcRepository;
import com.autel.cloud.pile.base.enums.AppTypeEnum;
import com.autel.cloud.pile.base.enums.PileBaseEnum;
import com.autel.cloud.pile.base.infrastructure.feign.CommonServiceClient;
import com.autel.cloud.pile.base.infrastructure.mapper.TbVirtualCardNfcMapper;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardExcelEntity;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.TbVirtualCardNfcEntity;
import com.autel.cloud.pile.base.vo.Attachment;
import com.autel.cloud.pile.base.vo.TbVirtualCardNfcVO;
import com.baomidou.mybatisplus.core.conditions.query.LambdaQueryWrapper;
import com.baomidou.mybatisplus.extension.service.impl.ServiceImpl;

import de.brendamour.jpasskit.PKField;
import de.brendamour.jpasskit.PKNFC;
import de.brendamour.jpasskit.PKPass;
import de.brendamour.jpasskit.passes.PKStoreCard;
import de.brendamour.jpasskit.signing.PKFileBasedSigningUtil;
import de.brendamour.jpasskit.signing.PKSigningInformation;
import de.brendamour.jpasskit.signing.PKSigningInformationUtil;
import lombok.extern.slf4j.Slf4j;
import org.apache.commons.io.IOUtils;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Service;
import org.springframework.util.ObjectUtils;
import org.springframework.mock.web.MockMultipartFile;
import org.springframework.web.multipart.MultipartFile;

import javax.annotation.Resource;
import java.io.*;
import java.net.URL;
import java.text.SimpleDateFormat;
import java.util.*;

/**
 * @author A22587
 */
@Slf4j
@Service
public class TbVirtualCardNfcRepositoryImpl extends ServiceImpl<TbVirtualCardNfcMapper, TbVirtualCardNfcEntity> implements TbVirtualCardNfcRepository {
    @Resource
    private CommonServiceClient commonServiceClient;

    @Override
    public TbVirtualCardNfcVO getNfcCardNumber(Integer nfcType, Long userId) {
        TbVirtualCardNfcVO tbVirtualCardNfcVO = new TbVirtualCardNfcVO();
        //查询有没有
        LambdaQueryWrapper<TbVirtualCardNfcEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbVirtualCardNfcEntity::getDeleted, 0)
                .eq(TbVirtualCardNfcEntity::getNfcType, nfcType)
                .eq(TbVirtualCardNfcEntity::getUserId, userId);
        TbVirtualCardNfcEntity tbVirtualCardNfcEntity = this.baseMapper.selectOne(queryWrapper);
        if (!ObjectUtils.isEmpty(tbVirtualCardNfcEntity)) {
            log.info("===>>>查询结果：{}", JSON.toJSONString(tbVirtualCardNfcEntity));
            //有就直接返回
            BeanUtils.copyProperties(tbVirtualCardNfcEntity, tbVirtualCardNfcVO);
            return tbVirtualCardNfcVO;
        }
        //没有就新增
        tbVirtualCardNfcEntity = new TbVirtualCardNfcEntity();
        tbVirtualCardNfcEntity.setNfcType(nfcType);
        tbVirtualCardNfcEntity.setUserId(userId);
        tbVirtualCardNfcEntity.setCreateTime(System.currentTimeMillis());
        tbVirtualCardNfcEntity.setUpdateTime(System.currentTimeMillis());
        tbVirtualCardNfcEntity.setDeleted(0);
        String cardNumber = Long.toHexString(userId).toUpperCase();
        tbVirtualCardNfcEntity.setCardNumber(cardNumber);
        if (nfcType.equals(AppTypeEnum.IOS_CODE.getValue())) {
            tbVirtualCardNfcEntity.setCardNumber("AP" + cardNumber);
            //ios类型的NFC卡需要生成.pkpass文件，并保存下载路径

//            String resource = "/sharing/energy-cloud-pile-base/pkpass/AWDRCA.pem";
//            String privateKeyPath = "/sharing/energy-cloud-pile-base/pkpass/passCertificate.p12";
//
//            String privateKeyPassword = "123456"; // the password you used to export
//            FileOutputStream fileOutputStream = null;
//            try {
//                PKSigningInformation pkSigningInformation = null;
//                if (privateKeyPath != null && resource != null) {
//                    pkSigningInformation = new PKSigningInformationUtil().
//                            loadSigningInformationFromPKCS12AndIntermediateCertificate(
//                                    privateKeyPath, privateKeyPassword, resource);
//                }
//                List<Long> longList = new ArrayList<>();
//                longList.add(1578454464L);
//                PKField pkField = PKField.builder()
//                        .key("deal")
//                        .label("name")
//                        .value("Shi BaoSheng")
//                        .build();
//                Date millisecondDate= new Date(System.currentTimeMillis());
//                //格式化时间
//                SimpleDateFormat formatter = new SimpleDateFormat("yyyy-MM-dd");
//                String millisecondStrings = formatter.format(millisecondDate);
//                PKField pkField2 = PKField.builder()
//                        .key("expiration")
//                        .label("addTime")
//                        .value(millisecondStrings)
//                        .build();
//                PKNFC pknfc = PKNFC.builder()
//                        .message("7IAPG0IrtESVykcECCK56P")
//                        .encryptionPublicKey("MDkwEwYHKoZIzj0CAQYIKoZIzj0DAQcDIgACigjq+QYC17m3i9uO8uKc8mLpaS1UJOEaCFvMedkXsuA=")
//                        .build();
//                //序列号，后台生成，每个用户都不同
//                String serialNumber = "858585";
//                PKPass pass = PKPass.builder()
//                        .formatVersion(1)
//                        .passTypeIdentifier("pass.com.autel.charge")
//                        .serialNumber(serialNumber)
//                        .teamIdentifier("59598GCKNY")
//                        .associatedStoreIdentifiers(longList)
//                        .appLaunchURL("https://evcharging.autel.com")
//                        .organizationName("AutelCharge,Inc")
//                        .description("AutelCharge Card")
//                        .logoText("AutelCharge,Inc")
//                        .foregroundColor("rgb(255, 255, 255)")
//                        .backgroundColor("rgb(55, 117, 50)")
//                        .pass(PKStoreCard.builder().auxiliaryField(pkField).auxiliaryField(pkField2).build())
////                        .nfc(pknfc)
//                        //设置回调地址，用于通行证的更新
////                        .webServiceURL(new URL("https://baidu.com"))
//                        .build();
//
//                log.info("===>>>PKPass对象：{}",JSON.toJSONString(pass));
//
//
//                String pathToTemplate = "/sharing/energy-cloud-pile-base/pkpass/StoreCardTest.raw";
//
//                PKFileBasedSigningUtil pkSigningUtil = new PKFileBasedSigningUtil();
//                byte[] passZipAsByteArray = new byte[0];
//                if (pathToTemplate != null) {
//                    passZipAsByteArray = pkSigningUtil.createSignedAndZippedPkPassArchive(pass, pathToTemplate, pkSigningInformation);
//                }
//
//                File file = File.createTempFile("mypass_", ".pkpass");
//                ByteArrayInputStream inputStream = new ByteArrayInputStream(passZipAsByteArray);
//                fileOutputStream = new FileOutputStream(file);
//                IOUtils.copy(inputStream, fileOutputStream);
//                //格式化时间
//                SimpleDateFormat formatterFileName = new SimpleDateFormat("yyyy_MM_dd");
//                String fileName = formatterFileName.format(millisecondDate);
//                String filePath = fileName + "_" + userId + "_ios_nfc" + file.getName() ;
//
//                MockMultipartFile multipartFile;
//                multipartFile = new MockMultipartFile("file", file.getName(), CharsetUtil.UTF_8, new FileInputStream(file));
//                Result<Attachment> amazone = commonServiceClient.upload(multipartFile, "amazone", filePath);
//                if (amazone == null || amazone.getData() == null) {
//                    throw new MessageCodeException(PileBaseEnum.UPLOAD_IOS_NFC_FILE_FAIL);
//                }
//                log.info(JSON.toJSONString(amazone));
//                tbVirtualCardNfcEntity.setIosDownloadUrl(amazone.getData().getPath());
//
//            } catch (Exception e) {
//                log.info("pkpass文件生成出错了：{}", e);
//            } finally {
//                if (fileOutputStream != null) {
//                    try {
//                        fileOutputStream.close();
//                    } catch (IOException e) {
//                    log.error("fileOutputStream关闭失败：{}", e);
//                    }
//                }
//            }
        }else if (nfcType.equals(AppTypeEnum.ANDROID_CODE.getValue())) {
            tbVirtualCardNfcEntity.setCardNumber("AN" + cardNumber);
        }
        this.baseMapper.insert(tbVirtualCardNfcEntity);
        log.info("===>>>新建虚拟卡号结果：{}", JSON.toJSONString(tbVirtualCardNfcEntity));
        BeanUtils.copyProperties(tbVirtualCardNfcEntity, tbVirtualCardNfcVO);
        return tbVirtualCardNfcVO;
    }

    @Override
    public Boolean validNFCCardNo(String cardNumber, String userId) {
        LambdaQueryWrapper<TbVirtualCardNfcEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbVirtualCardNfcEntity::getDeleted, 0)
                .eq(TbVirtualCardNfcEntity::getCardNumber, cardNumber)
                .eq(TbVirtualCardNfcEntity::getUserId, userId);
        TbVirtualCardNfcEntity tbVirtualCardNfcEntity = this.baseMapper.selectOne(queryWrapper);
        if (!ObjectUtils.isEmpty(tbVirtualCardNfcEntity)) {
            return Boolean.TRUE;
        }
        return Boolean.FALSE;
    }

    @Override
    public String uploadNFCCertificateModuleXls(MultipartFile file, String path) {
        // 获取文件原本的名字
        String originName = file.getOriginalFilename();
        // 保存文件的文件夹
        File folder = new File(path);
        // 判断路径是否存在,不存在则自动创建
        if(!folder.exists()){
            folder.mkdirs();
        }
        try {
            assert originName != null;
            file.transferTo(new File(folder, originName));
            String filePath = path + "\\" + originName;
            return new String("文件路径为:" + filePath);
        } catch (IOException e){
            return new String(e.getMessage());
        }
    }

    @Override
    public Result<TbVirtualCardNfcVO> getNfcCardInfoByNumber(String cardNumber) {
        TbVirtualCardNfcVO tbVirtualCardNfcVO = new TbVirtualCardNfcVO();
        //查询有没有
        LambdaQueryWrapper<TbVirtualCardNfcEntity> queryWrapper = new LambdaQueryWrapper<>();
        queryWrapper.eq(TbVirtualCardNfcEntity::getDeleted, 0)
                .eq(TbVirtualCardNfcEntity::getCardNumber, cardNumber);
        TbVirtualCardNfcEntity tbVirtualCardNfcEntity = this.baseMapper.selectOne(queryWrapper);
        if (!ObjectUtils.isEmpty(tbVirtualCardNfcEntity)) {
            log.info("===>>>查询结果：{}", JSON.toJSONString(tbVirtualCardNfcEntity));
            //有就直接返回
            BeanUtils.copyProperties(tbVirtualCardNfcEntity, tbVirtualCardNfcVO);
            return Result.ofSucceed(tbVirtualCardNfcVO);
        }
        return Result.ofSucceed(null);
    }
}
