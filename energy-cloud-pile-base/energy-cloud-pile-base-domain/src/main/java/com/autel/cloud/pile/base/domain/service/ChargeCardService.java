package com.autel.cloud.pile.base.domain.service;

import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.pile.base.dto.ChargeCardBusinessDTO;
import com.autel.cloud.pile.base.dto.ChargeCardBusinessPageDTO;
import com.autel.cloud.pile.base.dto.ChargeCardDTO;
import com.autel.cloud.pile.base.dto.ChargeCardOrderDTO;
import com.autel.cloud.pile.base.dto.*;
import com.autel.cloud.pile.base.infrastructure.mapper.entity.ChargeCardEntity;
import com.autel.cloud.pile.base.vo.*;
import com.baomidou.mybatisplus.core.metadata.IPage;
import com.baomidou.mybatisplus.extension.plugins.pagination.Page;
import com.baomidou.mybatisplus.extension.service.IService;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.multipart.MultipartFile;

import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

/**
 * @Author A22282
 * @Date 2022/5/9 17:18
 */
public interface ChargeCardService extends IService<ChargeCardEntity> {
    Result<Boolean> addCard(ChargeCardDTO chargeCardDTO);

    Result<Boolean> updateCard(ChargeCardDTO chargeCardDTO);

    Result<Boolean> removeCard(ChargeCardDTO chargeCardDTO);

    Result<List<ChargeCardVO>> cardList(ChargeCardDTO chargeCardDTO);

    String validCardNo(String cardNumber, String locationId, String evseId,Long operatorId);

    Result<String> addForBusiness(ChargeCardBusinessDTO cardBusinessDTO);

    Result<Boolean> updateForBusiness(ChargeCardBusinessDTO cardBusinessDTO);

    Result<IPage<ChargeCardBusinessVO>> pagesForBusiness(ChargeCardBusinessPageDTO cardBusinessDTO);

    Result<Boolean> deleteForBusiness(Long id);

    Result<Boolean> deleteUserData(Long userId);

    void download(HttpServletRequest request, HttpServletResponse response, Long sellerId);

    ChargeCardImportVO imports(HttpServletRequest request, MultipartFile file, Long sellerId);

    Integer updateBatchByCardNumber(List<ChargeCardEntity> datas);

    Result<ChargeCardInfoVO> findCard(String cardNumber,Long operatorId);

    boolean insertBatchByCardNumber(List<ChargeCardEntity> datas, boolean cover);

    Result<Boolean> syncCard(List<String> cardNumbers);

    Result<ChargeCardPageVO> getCardInfoByNumber(String cardNumber);

    Result<ChargeCardPageVO> getCardInfoByNumberAndUser(String cardNumber,Long userId,Long operatorId);

    Result<List<ChargeCardPageVO>> batchGetCardInfoByNumber(BatchQueryCardInfoParamDTO batchQueryCardInfoParamDTO);

    List<SimpleChargeCardVO> batchQueryCardList(List<String> cardNumberList);

    Page<ChargeCardEntity> batchQueryCardListForHubject(HashMap<String, Long> lastUpdate);

    Integer getSameNameCard(Long userId, String name, String cardNumber);

    Boolean syncCardName(Long userId, String name, String cardNumber);

    List<String> getCardNumberListByUser(String userId);

    String getCardNameByCardNumber(String cardNumber, String userId);

    Result<Boolean> bindCard(ChargeCardDTO chargeCardDTO);

    Result<Boolean> batchRemovalOfChargingCards(List<Long> ids);

    Result<Boolean> enableDisableChargingCard(EnableDisableChargingCardDTO enableDisableChargingCardDTO);

    Result<String> addForBusinessAdmin(ChargeCardBusinessDTO cardBusinessDTO);

    int check(String cardNumber,String name,Long sellerId);

    Result<ChargeCardBusinessVO> detail(Long id);

    Result<Boolean> syncChargeTimes(ChargeCardOrderDTO dto);

    Map<String, Object> getDisplaySetting(Long operatorId, Long userId);

    Map<String, Object> setDisplaySetting(@RequestBody ChargeCardDisPlayV2DTO ChargeCardDisPlayV2DTO);

    Map<String, Object> exchangeDisplayColumn(Long sellerId, Long userId, List<String> columnExchangerDTO);

    Result<IPage<ChargeCardPageVO>> pagesV2(ChargeCardPageDTO chargeCardPageDTO);

    List<ChargeCardEntity> findListV2(List<String> cardNumbers, Long sellerId);

    List<ChargeCardEntity> findList(List<String> cardAlias,Long sellerId);

    List<ChargeCardBusinessVO> findList(Long sellerId, Long driverId);

    void errorDetail(HttpServletRequest request, HttpServletResponse response, Long sellerId, Long code);

    boolean checkChargeCard(Long operatorId, String cardAlias, Long id);

    List<SimpleChargeCardVO> batchQueryCardListByUserIds(List<String> userIds);

    List<String> getAllCardIdBySellerId(Long sellerId);

    Result<List<Long>> searchBindCard(Long groupId);

    Boolean deletedAllDisPlaySetting();

    Result<IPage<CardOptionsPageVO>> cardOptionPageList(CardOptionsDTO cardOptionsDTO);
}
