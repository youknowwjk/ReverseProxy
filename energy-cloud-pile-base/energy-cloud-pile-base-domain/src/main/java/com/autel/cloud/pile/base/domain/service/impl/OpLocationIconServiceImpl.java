package com.autel.cloud.pile.base.domain.service.impl;

import cn.hutool.core.collection.CollUtil;
import com.alibaba.fastjson.JSON;
import com.autel.cloud.base.common.enums.ResultCodeEnum;
import com.autel.cloud.base.http.pojo.Result;
import com.autel.cloud.base.opencommons.util.LoginUserHolder;
import com.autel.cloud.base.opencommons.util.jwt.Payload;
import com.autel.cloud.pile.base.constant.BaseConstant;
import com.autel.cloud.pile.base.domain.repository.OpLocationIconRepository;
import com.autel.cloud.pile.base.domain.service.OpLocationIconService;
import com.autel.cloud.pile.base.dto.OpLocationIconDTO;
import com.autel.cloud.pile.base.vo.OpLocationIconVO;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.util.List;

/**
 * <p>
 * 图标信息（icon） 业务逻辑实现类
 * </p>
 *
 * @author A22599
 * @since 2022-07-28
 */
@Service
@Slf4j
public class OpLocationIconServiceImpl implements OpLocationIconService {

    @Autowired
    private OpLocationIconRepository opLocationIconRepository;

    /**
     * 获得所有图标信息（icon）条件查询
     *
     * @return
     */
    @Override
    public Result<List<OpLocationIconVO>> getAllIcon(OpLocationIconDTO opLocationIconDTO) {
        if (opLocationIconDTO.getLanguage() == null) {
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        List<OpLocationIconVO> opLocationIconVOS = opLocationIconRepository.getAllIcon(opLocationIconDTO);
        log.info("getAllIcon, output: opLocationIconVOS={}", JSON.toJSONString(opLocationIconVOS));
        if (opLocationIconVOS != null) {
            return Result.ofSucceed(opLocationIconVOS);
        }
        return Result.ofSucceed();
    }

    /**
     * 单个添加图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    @Override
    public Result<Boolean> addIcon(OpLocationIconDTO opLocationIconDTO) {
        log.info("addIcon, before checkOpLocationIconDTO, input: opLocationIconDTO={}", JSON.toJSONString(opLocationIconDTO));
        if (Boolean.FALSE.equals(this.checkOpLocationIconDTO(opLocationIconDTO))) {
            log.info("addIcon, after checkOpLocationIconDTO, input: opLocationIconDTO={}", JSON.toJSONString(opLocationIconDTO));
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        log.info("addIcon, after checkOpLocationIconDTO, input: opLocationIconDTO={}", JSON.toJSONString(opLocationIconDTO));
        Boolean result = opLocationIconRepository.getIconByCodeAndCategory(opLocationIconDTO);
        Integer integer = 0;
        if (Boolean.FALSE.equals(result)) {
            integer = opLocationIconRepository.addIcon(opLocationIconDTO);
        }
        log.info("addIcon, output: integer={}", JSON.toJSONString(integer));
        if (integer <= 0) {
            throw new RuntimeException("Description Failed to add icon information!");
        }
        return Result.ofSucceed(true);
    }

    /**
     * 单个修改图标信息（icon）
     *
     * @param opLocationIconDTO
     * @return
     */
    @Override
    public Result<Boolean> editIcon(OpLocationIconDTO opLocationIconDTO) {
        log.info("editIcon, before checkOpLocationIconDTO, input: opLocationIconDTO={}", JSON.toJSONString(opLocationIconDTO));
        if (Boolean.FALSE.equals(this.checkOpLocationIconDTO(opLocationIconDTO))) {
            log.info("editIcon, after checkOpLocationIconDTO, input: opLocationIconDTO={}", JSON.toJSONString(opLocationIconDTO));
            return Result.ofFailed(ResultCodeEnum.CUSTOMER_ERROR.setMessage(BaseConstant.PARAM_IS_NULL));
        }
        log.info("editIcon, after checkOpLocationIconDTO, input: opLocationIconDTO={}", JSON.toJSONString(opLocationIconDTO));
        Boolean result = opLocationIconRepository.getIconByCodeAndCategory(opLocationIconDTO);
        Integer integer = 0;
        if (Boolean.FALSE.equals(result)) {
            OpLocationIconVO opLocationIconVO = opLocationIconRepository.getIconById(opLocationIconDTO);
            if (opLocationIconVO.getId() != null) {
                integer = opLocationIconRepository.editIcon(opLocationIconDTO);
            }
        }
        log.info("editIcon, output: integer={}", JSON.toJSONString(integer));
        if (integer <= 0) {
            throw new RuntimeException("Failed to modify the icon. Procedure!");
        }
        return Result.ofSucceed(true);
    }

    /**
     * 初始化图标信息（icon）
     * 1.删除图标信息表（op_icon）已有的数据
     * 2.对opLocationIconDTO对象的属性进行完善（初始化数据）
     * 3.批量插入初始数据
     *
     * @return
     */
    @Transactional(rollbackFor = {Exception.class})
    @Override
    public Result<Boolean> initializeIconInformation() {
        try {
            List<Long> opLocationIconIdList = opLocationIconRepository.selectAllIconId();
            if (CollUtil.isNotEmpty(opLocationIconIdList)) {
                opLocationIconRepository.deleteIconByIdList(opLocationIconIdList);
            }
            Payload payload = LoginUserHolder.getLoginUser().getPayload();
            for (int i = 1; i <= 7; i++) {
                for (int j = 1; j <= 22; j++) {
                    OpLocationIconDTO opLocationIconDTO = new OpLocationIconDTO();
                    opLocationIconDTO.setCreatedBy(payload.getUserId());
                    opLocationIconDTO.setCreatedAt(System.currentTimeMillis());
                    opLocationIconDTO.setUpdatedBy(payload.getUserId());
                    opLocationIconDTO.setUpdatedAt(System.currentTimeMillis());
                    opLocationIconDTO.setDeleted(0);
                    opLocationIconDTO.setCategory("1");
                    switch (i) {
                        case 1: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552533653805686785-blob");
                            opLocationIconDTO.setCode("1");
                            opLocationIconDTO.setDesc("餐饮");
                            break;
                        }
                        case 2: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552533299764486145-blob");
                            opLocationIconDTO.setCode("2");
                            opLocationIconDTO.setDesc("无线网络");
                            break;
                        }
                        case 3: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552533206114066434-blob");
                            opLocationIconDTO.setCode("3");
                            opLocationIconDTO.setDesc("卫生间");
                            break;
                        }
                        case 4: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552533480111169538-blob");
                            opLocationIconDTO.setCode("4");
                            opLocationIconDTO.setDesc("住宿");
                            break;
                        }
                        case 5: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552533093501198337-blob");
                            opLocationIconDTO.setCode("5");
                            opLocationIconDTO.setDesc("停车场");
                            break;
                        }
                        case 6: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552532938173538306-blob");
                            opLocationIconDTO.setCode("6");
                            opLocationIconDTO.setDesc("购物");
                            break;
                        }
                        case 7: {
                            opLocationIconDTO.setIconUrl("https://s3.cn-northwest-1.amazonaws.com.cn/default.enetest/1552533398175440898-blob");
                            opLocationIconDTO.setCode("7");
                            opLocationIconDTO.setDesc("娱乐");
                            break;
                        }
                        default:
                            break;
                    }
                    switch (j) {
                        case 1: {
                            opLocationIconDTO.setLanguage("da-DK");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Mad");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName(BaseConstant.TOILET);
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Indkvartering");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkeringsplads");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Indkøbscenter");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Underholdning");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 2: {
                            opLocationIconDTO.setLanguage("de-DE");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName(BaseConstant.RESTAURANT);
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("WLAN");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Toilette");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Unterkunft");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkplatz");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Mall");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Entertainment");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 3: {
                            opLocationIconDTO.setLanguage("el-GR");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Τροφή");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Τουαλέτα");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Κατάλυμα");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Θέση στάθμευσης");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Εμπορικό κέντρο");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Διασκέδαση");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 4: {
                            opLocationIconDTO.setLanguage("en-US");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName(BaseConstant.RESTAURANT);
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName(BaseConstant.TOILET);
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Accommodation");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parking Lot");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Mall");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Entertainment");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 5: {
                            opLocationIconDTO.setLanguage("es-ES");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Restaurante");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Baño");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Alojamiento");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Aparcamiento");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Centro comercial");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Entretenimiento");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 6: {
                            opLocationIconDTO.setLanguage("fi-FL");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Ruoka");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("WC");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Majoitus");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkkipaikka");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Ostoskeskus");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Viihde");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 7: {
                            opLocationIconDTO.setLanguage("fr-FR");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName(BaseConstant.RESTAURANT);
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Toilette");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Logement");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parking");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Centre commercia");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Divertissement");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 8: {
                            opLocationIconDTO.setLanguage("hu-HU");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Élelmiszer");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("WiFi");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Mellékhelyiség");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Szállás");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkoló");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Bevásárlóközpont");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Szórakozás");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 9: {
                            opLocationIconDTO.setLanguage("it-IT");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Ristorante");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Bagno");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Alloggio");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parcheggio");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Centro commerciale");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Intrattenimento");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 10: {
                            opLocationIconDTO.setLanguage("iw-IL");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("מזון");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("וויי - פיי");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("שירותים");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("דִיוּר");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("מגרש חניה");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("קניון");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("בידור");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 11: {
                            opLocationIconDTO.setLanguage("ja-JP");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("フード");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("トイレ");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("アコモデーション");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("駐車場");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("モール");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("エンターテインメント");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 12: {
                            opLocationIconDTO.setLanguage("ko-KR");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("음식");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("와이파이");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("화장실");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("숙소");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("주차장");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("몰");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("오락");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 13: {
                            opLocationIconDTO.setLanguage("nl-NL");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Eten");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("WiFi");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName(BaseConstant.TOILET);
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Accommodatie");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkeerplaats");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Winkelcentrum");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Amusement");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 14: {
                            opLocationIconDTO.setLanguage("no-NO");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Mat");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Toalett");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Overnatting");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkeringsplass");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("kjøpesenter");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Underholdning");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 15: {
                            opLocationIconDTO.setLanguage("pl-PL");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Jedzenie");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Toaleta");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Akomodacja");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parking");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Centrum handlowe");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Rozrywka");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 16: {
                            opLocationIconDTO.setLanguage("pt-PT");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Comida");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("W/C");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Acomodação");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Estacionamento");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Centro comercial");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Entretenimento");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 17: {
                            opLocationIconDTO.setLanguage("ru-RU");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Продукты питания");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Туалет");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Размещение");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Парковка");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Торговый центр");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Развлечения");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 18: {
                            opLocationIconDTO.setLanguage("sv-SE");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Mat");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Toalett");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Boende");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Parkeringsplats");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("Köpcenter");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Underhåll");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 19: {
                            opLocationIconDTO.setLanguage("th-TH");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("อาหาร");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("ห้องน้ำ");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("ที่พัก");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("ลานจอดรถ");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("ห้างสรรพสินค้า");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("ความบันเทิง");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 20: {
                            opLocationIconDTO.setLanguage("tr-TR");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("Restorant");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName(BaseConstant.WI_FI);
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("Tuvalet");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("Konaklama");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("Park Yeri");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("AVM");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("Eğlence");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 21: {
                            opLocationIconDTO.setLanguage("zh-CN");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("餐饮");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("无线网络");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("卫生间");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("住宿");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("停车场");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("购物");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("娱乐");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        case 22: {
                            opLocationIconDTO.setLanguage("zh-TW");
                            switch (i) {
                                case 1: {
                                    opLocationIconDTO.setName("餐飲");
                                    break;
                                }
                                case 2: {
                                    opLocationIconDTO.setName("無線網絡");
                                    break;
                                }
                                case 3: {
                                    opLocationIconDTO.setName("衛生間");
                                    break;
                                }
                                case 4: {
                                    opLocationIconDTO.setName("住宿");
                                    break;
                                }
                                case 5: {
                                    opLocationIconDTO.setName("停車場");
                                    break;
                                }
                                case 6: {
                                    opLocationIconDTO.setName("購物");
                                    break;
                                }
                                case 7: {
                                    opLocationIconDTO.setName("娛樂");
                                    break;
                                }
                                default:
                                    break;
                            }
                            break;
                        }
                        default:
                            break;
                    }
                    opLocationIconRepository.addIcon(opLocationIconDTO);
                }
            }
        } catch (Exception e) {
            log.error("OpLocationIconServiceImpl-initializeIconInformation:" + e);
            throw new RuntimeException("Failed to initialize icon data. Procedure!");
        }
        return Result.ofSucceed(true);
    }

    /**
     * 1.校验opLocationIconDTO对象的属性
     * 2.根据主键（id）是否存在判断是新增操作还是修改操作
     * 3.根据不同的操作类型（新增，修改）对opLocationIconDTO对象的属性进行完善
     *
     * @param opLocationIconDTO
     * @return
     */
    private Boolean checkOpLocationIconDTO(OpLocationIconDTO opLocationIconDTO) {
        if (opLocationIconDTO == null) {
            // 对象为空，拒接
            return false;
        }
        if (opLocationIconDTO.getCode() == null || opLocationIconDTO.getName() == null || opLocationIconDTO.getIconUrl() == null || opLocationIconDTO.getLanguage() == null) {
            // 必填字段为空，拒接
            return false;
        }
        Payload payload = LoginUserHolder.getLoginUser().getPayload();
        // 判断是新增操作还是修改操作
        if (opLocationIconDTO.getId() != null) {
            // 说明是修改操作
            opLocationIconDTO.setUpdatedBy(payload.getUserId());
            opLocationIconDTO.setUpdatedAt(System.currentTimeMillis());
        } else {
            // 说明是新增操作
            opLocationIconDTO.setCreatedBy(payload.getUserId());
            opLocationIconDTO.setCreatedAt(System.currentTimeMillis());
            opLocationIconDTO.setUpdatedBy(payload.getUserId());
            opLocationIconDTO.setUpdatedAt(System.currentTimeMillis());
        }
        if (opLocationIconDTO.getDeleted() == null || opLocationIconDTO.getCategory() == null) {
            opLocationIconDTO.setDeleted(0);
            opLocationIconDTO.setCategory("1");
        }
        return true;
    }
}
