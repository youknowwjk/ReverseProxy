package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.*;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Map;

/**
 * @description
 * @auther A23204
 * @datetime 2023/8/23 16:48
 */
public interface SubscribeImportAutoBindService {

    /**
     * description: startImport  商家桩导入
     * version: 1.0
     * date: 2023/8/23 16:55
     * author: A23204
     *
     * @param multipartFile
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map<java.lang.String,java.util.List<java.lang.String>>>
     */
    Result<SubscribeImportResp> startImport(MultipartFile multipartFile);

    /**
     * description: queryImportFailedSns 查询导入失败的桩信息 从缓存读取
     * version: 1.0
     * date: 2023/8/23 16:58
     * author: A23204
     *
     * @param
     * @return com.autel.cloud.base.http.pojo.Result<java.util.Map<java.lang.String,java.util.List<java.lang.String>>>
     */
    Result<List<SubscribeImportFailed>> queryImportFailedSns();

    /**
     * description: bindLicenceManual 手动绑定下PO单商家的桩
     * version: 1.0
     * date: 2023/8/28 8:55
     * author: A23204
     *
     * @param subscribeBindManualReq
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */
    Result<List<SubscribeBindManualDto>> bindLicenceManual(SubscribeBindManualReq subscribeBindManualReq);

    /**
     * description: defaultSellerLicenceImport  默认一个月的商家重新导入
     * version: 1.0
     * date: 2023/9/17 14:23 
     * author: A23204 
     * 
     * @param multipartFile
     * @return com.autel.cloud.base.http.pojo.Result<com.autel.cloud.pile.base.dto.SubscribeImportResp>
     */ 
    Result<SubscribeImportResp> defaultSellerLicenceImport(MultipartFile multipartFile);

    /**
     * description: deleteDefaultOneMonth  deleteDefaultOneMonth
     * version: 1.0
     * date: 2023/9/18 9:21 
     * author: A23204 
     * 
     * @param pileSnList
     * @return com.autel.cloud.base.http.pojo.Result<java.lang.Boolean>
     */ 
    Result<Boolean> deleteDefaultOneMonth(List<String> pileSnList);
}
