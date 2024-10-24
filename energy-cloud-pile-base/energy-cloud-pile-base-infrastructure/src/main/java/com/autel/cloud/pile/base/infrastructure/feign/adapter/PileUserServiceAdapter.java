package com.autel.cloud.pile.base.infrastructure.feign.adapter;

import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.JwtInfo;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.pile.base.enums.MerchantType;
import com.autel.cloud.pile.user.api.feign.PileMerchantUserFeign;
import com.autel.cloud.pile.user.api.feign.PileUserFeign;
import com.autel.cloud.pile.user.api.utils.LoginUserUtil;
import com.autel.cloud.pile.user.api.vo.MemberGroupVO;
import com.autel.cloud.pile.user.api.vo.MemberVO;
import com.autel.cloud.pile.user.api.vo.SellerDetailVO;
import com.google.common.cache.CacheBuilder;
import com.google.common.cache.CacheLoader;
import com.google.common.cache.LoadingCache;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.stereotype.Component;
import org.springframework.util.CollectionUtils;

import javax.annotation.Resource;
import java.util.*;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.TimeUnit;

@Slf4j
@Component
public class PileUserServiceAdapter extends AbstractFeignServiceAdapter {

    private final LoadingCache<String, Collection<Long>> locationIdByUserIdCache = CacheBuilder.newBuilder()
            .expireAfterWrite(10, TimeUnit.MINUTES)
            .initialCapacity(100)
            .maximumSize(1000)
            .build(new CacheLoader<String, Collection<Long>>() {
                @Override
                public Collection<Long> load(String id) {
                    return getLocationIdsByUserId(id);
                }
            });

    @Resource
    private PileMerchantUserFeign pileMerchantUserFeign;

    @Resource
    private PileUserFeign pileUserFeign;


    @Value("${webhook.wechat.key.pile-user:66ae591a-4e15-4b57-9a1c-56978ba6152b}")
    protected String webhookWechatKey;

    private final LoadingCache<Long, SellerDetailVO> sellerCache = CacheBuilder.newBuilder()
            .expireAfterWrite(30, TimeUnit.MINUTES)
            .initialCapacity(100)
            .maximumSize(1000)
            .build(new CacheLoader<Long, SellerDetailVO>() {
                @Override
                public SellerDetailVO load(Long id) {
                    return findMerchantByIdSupportFallback(id);
                }
            });


    public SellerDetailVO findMerchantById(Long sellerId) {
        log.info("findMerchantUserById: {}", sellerId);
        Result<SellerDetailVO> listResult = pileMerchantUserFeign.detail(sellerId);
        log.info("findMerchantUserById result: {}", JSON.toJSONString(listResult));
        SellerDetailVO handle = nullableHandle(listResult);
        if (Objects.isNull(handle)) {
            String content = String.format("调用服务(pile-user-app/seller/detail)没有查询该商家 \n sellerId=%s", sellerId);
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return null;
        }
        return handle;
    }


    public MerchantType getMerchantTypeById(Long sellerId) {
        SellerDetailVO sellerDetailVO = getMerchantByIdSupportFallback(sellerId);
        return MerchantType.keyOf(sellerDetailVO.getType());
    }


    public SellerDetailVO findMerchantByIdSupportFallback(Long sellerId) {
        log.info("findMerchantByIdSupportFallback: {}", sellerId);
        try {
            Result<SellerDetailVO> listResult = pileMerchantUserFeign.detail(sellerId);
            log.info("findMerchantByIdSupportFallback result: {}", JSON.toJSONString(listResult));
            SellerDetailVO handle = nullableHandle(listResult);
            if (Objects.isNull(handle)) {
                String content = String.format("调用服务(pile-user-app/seller/detail)没有查询该商家 \n sellerId=%s", sellerId);
                weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
                SellerDetailVO sellerDetailVO = new SellerDetailVO();
                sellerDetailVO.setId(sellerId.toString());
                sellerDetailVO.setName("--");
                sellerDetailVO.setAccountName("--");
                sellerDetailVO.setType(3);
                return sellerDetailVO;
            }
            return handle;
        } catch (Exception e) {
            SellerDetailVO sellerDetailVO = new SellerDetailVO();
            sellerDetailVO.setName("--");
            sellerDetailVO.setAccountName("--");
            sellerDetailVO.setType(3);
            return sellerDetailVO;
        }
    }

    public SellerDetailVO getMerchantByIdSupportFallback(Long sellerId) {
        if (Objects.isNull(sellerId)) {
            SellerDetailVO sellerDetailVO = new SellerDetailVO();
            sellerDetailVO.setName("--");
            sellerDetailVO.setAccountName("--");
            sellerDetailVO.setType(3);
            return sellerDetailVO;
        }
        try {
            return sellerCache.get(sellerId);
        } catch (ExecutionException e) {
            log.error("getMerchantByIdSupportFallback", e);
            return findMerchantByIdSupportFallback(sellerId);
        }
    }

    /**
     * 获取用户的场站ID
     *
     * @return 场站IDs  至少会有一个不存在的值 防止出错
     */
    public Collection<Long> getLocationIdsByUserId() {
        JwtInfo loginUser = LoginUserHolder.getLoginUser();
        Long sellerId = LoginUserUtil.getSellerId();
        try {
            return locationIdByUserIdCache.get(loginUser.getId() + ":" + sellerId);
        } catch (ExecutionException e) {
            log.error("getLocationIdsByUserId" + JSON.toJSONString(loginUser), e);
        }
        return getLocationIdsByUserId(loginUser.getId() + ":" + sellerId);
    }

    private Collection<Long> getLocationIdsByUserId(String userId) {
        JwtInfo loginUser = LoginUserHolder.getLoginUser();
        Result<List<Long>> result = pileUserFeign.getLocationIds();
        List<Long> handle = nullableHandle(result);
        if (CollectionUtils.isEmpty(handle)) {
            String content = String.format("调用服务(pile-user-app/user/getLocationIds)没有查询该用户下的场站 \n userId=%s  nickname=%s account=%s",
                    userId, loginUser.getNickname(), loginUser.getPayload().getAccount());
            weChatClient.sendMessage(buildTextMessage(content), webhookWechatKey);
            return Collections.singletonList(Long.MIN_VALUE);
        }
        return new HashSet<>(handle);
    }


    /**
     * 判断当前用户是否是商户管理员
     *
     * @return 是 true
     */
    public boolean isMerchantAdmin() {
        return LoginUserUtil.isSellerAdmin();
    }

    public List<MemberVO> findMemberByIds(Set<Long> ids) {
        log.info("findMemberByIds params {}", ids);
        Result<List<MemberVO>> result = pileUserFeign.findMemberByIds(ids);
        log.info("findMemberByIds result {}", result);
        return Optional.ofNullable(nullableHandle(result)).orElse(Collections.emptyList());
    }

    public List<MemberGroupVO> findMemberGroupByIds(Set<Long> ids) {
        log.info("findMemberGroupByIds params {}", ids);
        Result<List<MemberGroupVO>> result = pileUserFeign.findMemberGroupByIds(ids);
        log.info("findMemberGroupByIds result {}", result);
        return Optional.ofNullable(nullableHandle(result)).orElse(Collections.emptyList());
    }

    public void refreshSeller(Long sellerId) {
        sellerCache.refresh(sellerId);
    }

    public void cleanUp() {
        sellerCache.cleanUp();
    }
}
