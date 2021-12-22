package org.im97mori.ble.android.peripheral.ui.device.setting.u180a;

import static org.im97mori.ble.constants.CharacteristicUUID.MANUFACTURER_NAME_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.MODEL_NUMBER_STRING_CHARACTERISTIC;
import static org.im97mori.ble.constants.CharacteristicUUID.SYSTEM_ID_CHARACTERISTIC;
import static org.im97mori.ble.constants.ServiceUUID.DEVICE_INFORMATION_SERVICE;

import android.bluetooth.BluetoothGattService;
import android.content.Intent;

import androidx.annotation.MainThread;
import androidx.annotation.NonNull;
import androidx.annotation.Nullable;
import androidx.lifecycle.LifecycleOwner;
import androidx.lifecycle.MutableLiveData;
import androidx.lifecycle.Observer;
import androidx.lifecycle.SavedStateHandle;
import androidx.lifecycle.Transformations;

import com.google.gson.Gson;
import com.google.gson.JsonSyntaxException;

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseServiceSettingViewModel;
import org.im97mori.ble.android.peripheral.utils.ExistObserver;
import org.im97mori.ble.characteristic.u2a23.SystemId;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;

import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.core.Single;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class DeviceInformationServiceSettingViewModel extends BaseServiceSettingViewModel {

    private static final String KEY_IS_SYSTEM_ID_SUPPORTED = "KEY_IS_SYSTEM_ID_SUPPORTED";

    private static final String KEY_MANUFACTURER_NAME_STRING_DATA_JSON = "KEY_MANUFACTURER_NAME_STRING_DATA_JSON";
    private static final String KEY_MODEL_NUMBER_STRING_DATA_JSON = "KEY_MODEL_NUMBER_STRING_DATA_JSON";
    private static final String KEY_SYSTEM_ID_DATA_JSON = "KEY_SYSTEM_ID_DATA_JSON";

    private static final String KEY_MANUFACTURER_NAME_STRING = "KEY_MANUFACTURER_NAME_STRING";
    private static final String KEY_MODEL_NUMBER_STRING = "KEY_MODEL_NUMBER_STRING";
    private static final String KEY_MANUFACTURER_IDENTIFIER = "KEY_MANUFACTURER_IDENTIFIER";
    private static final String KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER = "KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER";

    private final SavedStateHandle mSavedStateHandle;

    private final MutableLiveData<Boolean> mIsSystemIdSupported;

    private final MutableLiveData<String> mManufacturerNameStringDataJson;
    private final MutableLiveData<String> mModelNumberStringDataJson;
    private final MutableLiveData<String> mSystemIdDataJson;

    @Inject
    public DeviceInformationServiceSettingViewModel(@NonNull DeviceRepository deviceRepository, @NonNull SavedStateHandle savedStateHandle, @NonNull Gson gson) {
        super(deviceRepository, gson);
        mSavedStateHandle = savedStateHandle;

        mIsSystemIdSupported = savedStateHandle.getLiveData(KEY_IS_SYSTEM_ID_SUPPORTED);

        mManufacturerNameStringDataJson = savedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING_DATA_JSON);
        mModelNumberStringDataJson = savedStateHandle.getLiveData(KEY_MODEL_NUMBER_STRING_DATA_JSON);
        mSystemIdDataJson = savedStateHandle.getLiveData(KEY_SYSTEM_ID_DATA_JSON);
    }

    @NonNull
    public Completable setup(@NonNull Intent intent) {
        return Completable.create(emitter -> {
            if (mServiceData == null) {
                String dataJson = intent.getStringExtra(DEVICE_INFORMATION_SERVICE.toString());
                try {
                    mServiceData = mGson.fromJson(dataJson, ServiceData.class);
                } catch (JsonSyntaxException e) {
                    e.printStackTrace();
                }

                if (mServiceData == null) {
                    mServiceData = new ServiceData();
                    mServiceData.uuid = DEVICE_INFORMATION_SERVICE;
                    mServiceData.type = BluetoothGattService.SERVICE_TYPE_PRIMARY;
                }

                Optional<CharacteristicData> manufacturerNameStringOptional = mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> characteristicData.uuid.equals(MANUFACTURER_NAME_STRING_CHARACTERISTIC))
                        .findAny();

                Optional<CharacteristicData> modelNumberStringOptional = mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> characteristicData.uuid.equals(MODEL_NUMBER_STRING_CHARACTERISTIC))
                        .findAny();

                Optional<CharacteristicData> systemIdOptional = mServiceData.characteristicDataList
                        .stream()
                        .filter(characteristicData -> characteristicData.uuid.equals(SYSTEM_ID_CHARACTERISTIC))
                        .findAny();

                if (mManufacturerNameStringDataJson.getValue() == null) {
                    if (manufacturerNameStringOptional.isPresent()) {
                        CharacteristicData characteristicData = manufacturerNameStringOptional.get();
                        mManufacturerNameStringDataJson.postValue(mGson.toJson(characteristicData));
                        if (characteristicData.data != null) {
                            mSavedStateHandle.<String>getLiveData(KEY_MANUFACTURER_NAME_STRING)
                                    .postValue(new ManufacturerNameString(characteristicData.data).getManufacturerName());
                        }
                    }
                }

                if (mModelNumberStringDataJson.getValue() == null) {
                    if (modelNumberStringOptional.isPresent()) {
                        CharacteristicData characteristicData = modelNumberStringOptional.get();
                        mModelNumberStringDataJson.postValue(mGson.toJson(characteristicData));
                        if (characteristicData.data != null) {
                            mSavedStateHandle.<String>getLiveData(KEY_MODEL_NUMBER_STRING)
                                    .postValue(new ManufacturerNameString(characteristicData.data).getManufacturerName());
                        }
                    }
                }

                if (mIsSystemIdSupported.getValue() == null) {
                    mIsSystemIdSupported.postValue(systemIdOptional.isPresent());
                }

                if (mSystemIdDataJson.getValue() == null) {
                    if (systemIdOptional.isPresent()) {
                        CharacteristicData characteristicData = systemIdOptional.get();
                        mSystemIdDataJson.postValue(mGson.toJson(characteristicData));
                        if (characteristicData.data != null) {
                            SystemId systemId = new SystemId(characteristicData.data);
                            mSavedStateHandle.<String>getLiveData(KEY_MANUFACTURER_IDENTIFIER)
                                    .postValue(String.valueOf(systemId.getManufacturerIdentifier()));
                            mSavedStateHandle.<String>getLiveData(KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER)
                                    .postValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
                        }
                    }
                }
                emitter.onComplete();
            } else {
                emitter.onError(new RuntimeException("Initialized"));
            }
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

    @MainThread
    public void observeHasManufacturerNameStringDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mManufacturerNameStringDataJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasModelNumberStringDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mModelNumberStringDataJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasSystemIdDataJson(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSystemIdDataJson).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeIsSystemIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsSystemIdSupported).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_MANUFACTURER_NAME_STRING)).observe(owner, observer);
    }

    @MainThread
    public void observeModelNumberString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_MODEL_NUMBER_STRING)).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_MANUFACTURER_IDENTIFIER)).observe(owner, observer);
    }

    @MainThread
    public void observeOrganizationallyUniqueIdentifier(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER)).observe(owner, observer);
    }

    @MainThread
    public void updateIsSystemIdSupported(@NonNull Boolean checked) {
        mIsSystemIdSupported.setValue(checked);
    }

    @Nullable
    @MainThread
    public String getManufacturerNameStringDataJson() {
        return mManufacturerNameStringDataJson.getValue();
    }

    @MainThread
    public void setManufacturerNameStringDataJson(@Nullable String manufacturerNameStringDataJson) {
        mManufacturerNameStringDataJson.setValue(manufacturerNameStringDataJson);
        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING);
        if (manufacturerNameStringDataJson == null) {
            liveData.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(manufacturerNameStringDataJson, CharacteristicData.class);
                if (characteristicData.data == null) {
                    liveData.setValue(null);
                } else {
                    liveData.setValue(new ManufacturerNameString(characteristicData.data).getManufacturerName());
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    @MainThread
    public String getModelNumberStringDataJson() {
        return mModelNumberStringDataJson.getValue();
    }

    @MainThread
    public void setModelNumberStringDataJson(@Nullable String modelNumberStringDataJson) {
        mModelNumberStringDataJson.setValue(modelNumberStringDataJson);
        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_MODEL_NUMBER_STRING);
        if (modelNumberStringDataJson == null) {
            liveData.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(modelNumberStringDataJson, CharacteristicData.class);
                if (characteristicData.data == null) {
                    liveData.setValue(null);
                } else {
                    liveData.setValue(new ModelNumberString(characteristicData.data).getModelNumber());
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @Nullable
    @MainThread
    public String getSystemIdDataJson() {
        return mSystemIdDataJson.getValue();
    }

    @MainThread
    public void setSystemIdDataJson(@Nullable String systemIdDataJson) {
        mSystemIdDataJson.setValue(systemIdDataJson);
        MutableLiveData<String> manufacturerIdentifierLiveData = mSavedStateHandle.getLiveData(KEY_MANUFACTURER_IDENTIFIER);
        MutableLiveData<String> organizationallyUniqueIdentifierLiveData = mSavedStateHandle.getLiveData(KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER);
        if (systemIdDataJson == null) {
            manufacturerIdentifierLiveData.setValue(null);
            organizationallyUniqueIdentifierLiveData.setValue(null);
        } else {
            try {
                CharacteristicData characteristicData = mGson.fromJson(systemIdDataJson, CharacteristicData.class);
                if (characteristicData.data == null) {
                    manufacturerIdentifierLiveData.setValue(null);
                    organizationallyUniqueIdentifierLiveData.setValue(null);
                } else {
                    SystemId systemId = new SystemId(characteristicData.data);
                    manufacturerIdentifierLiveData.setValue(String.valueOf(systemId.getManufacturerIdentifier()));
                    organizationallyUniqueIdentifierLiveData.setValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
                }
            } catch (JsonSyntaxException e) {
                e.printStackTrace();
            }
        }
    }

    @NonNull
    @Override
    public Single<Intent> save() {
        return Single.<Intent>create(emitter -> {
                    ServiceData serviceData = mServiceData;
                    if (serviceData == null) {
                        emitter.onError(new RuntimeException("Already saved"));
                    } else {
                        serviceData.characteristicDataList.clear();

                        String manufacturerNameStringJson = mManufacturerNameStringDataJson.getValue();
                        if (manufacturerNameStringJson != null) {
                            try {
                                serviceData.characteristicDataList.add(mGson.fromJson(manufacturerNameStringJson, CharacteristicData.class));
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        String modelNumberStringJson = mModelNumberStringDataJson.getValue();
                        if (modelNumberStringJson != null) {
                            try {
                                serviceData.characteristicDataList.add(mGson.fromJson(modelNumberStringJson, CharacteristicData.class));
                            } catch (JsonSyntaxException e) {
                                e.printStackTrace();
                            }
                        }

                        if (Boolean.TRUE.equals(mIsSystemIdSupported.getValue())) {
                            String systemIdJson = mSystemIdDataJson.getValue();
                            if (systemIdJson != null) {
                                try {
                                    serviceData.characteristicDataList.add(mGson.fromJson(systemIdJson, CharacteristicData.class));
                                } catch (JsonSyntaxException e) {
                                    e.printStackTrace();
                                }
                            }
                        }

                        if (serviceData.characteristicDataList
                                .stream()
                                .filter(characteristicData -> !characteristicData.uuid.equals(SYSTEM_ID_CHARACTERISTIC)).count() == 2) {
                            Intent intent = new Intent();
                            intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), mGson.toJson(serviceData));
                            mServiceData = null;
                            emitter.onSuccess(intent);
                        } else {
                            emitter.onError(new RuntimeException("No data"));
                        }
                    }
                }
        )
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread());
    }

}