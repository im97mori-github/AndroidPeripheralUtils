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

import org.im97mori.ble.CharacteristicData;
import org.im97mori.ble.ServiceData;
import org.im97mori.ble.android.peripheral.hilt.repository.DeviceSettingRepository;
import org.im97mori.ble.android.peripheral.ui.device.setting.BaseServiceSettingViewModel;
import org.im97mori.ble.android.peripheral.utils.ExistObserver;
import org.im97mori.ble.android.peripheral.utils.Utils;
import org.im97mori.ble.characteristic.u2a23.SystemId;
import org.im97mori.ble.characteristic.u2a24.ModelNumberString;
import org.im97mori.ble.characteristic.u2a29.ManufacturerNameString;

import java.util.LinkedList;
import java.util.Optional;

import javax.inject.Inject;

import dagger.hilt.android.lifecycle.HiltViewModel;
import io.reactivex.rxjava3.android.schedulers.AndroidSchedulers;
import io.reactivex.rxjava3.core.Completable;
import io.reactivex.rxjava3.functions.Action;
import io.reactivex.rxjava3.functions.Consumer;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltViewModel
public class DeviceInformationServiceSettingViewModel extends BaseServiceSettingViewModel {

    private static final String KEY_IS_SYSTEM_ID_SUPPORTED = "KEY_IS_SYSTEM_ID_SUPPORTED";

    private static final String KEY_SYSTEM_ID_DATA = "KEY_SYSTEM_ID_DATA";
    private static final String KEY_MODEL_NUMBER_STRING_DATA = "KEY_MODEL_NUMBER_STRING_DATA";
    private static final String KEY_MANUFACTURER_NAME_STRING_DATA = "KEY_MANUFACTURER_NAME_STRING_DATA";

    private static final String KEY_MANUFACTURER_IDENTIFIER = "KEY_MANUFACTURER_IDENTIFIER";
    private static final String KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER = "KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER";
    private static final String KEY_MODEL_NUMBER_STRING = "KEY_MODEL_NUMBER_STRING";
    private static final String KEY_MANUFACTURER_NAME_STRING = "KEY_MANUFACTURER_NAME_STRING";

    private final SavedStateHandle mSavedStateHandle;

    private final MutableLiveData<Boolean> mIsSystemIdSupported;

    private final MutableLiveData<byte[]> mSystemIdData;
    private final MutableLiveData<byte[]> mModelNumberStringData;
    private final MutableLiveData<byte[]> mManufacturerNameStringData;

    private final MutableLiveData<String> mManufacturerIdentifier;
    private final MutableLiveData<String> mOrganizationallyUniqueIdentifier;
    private final MutableLiveData<String> mModelNumberString;
    private final MutableLiveData<String> mManufacturerNameString;

    private final MutableLiveData<Intent> mSavedData;

    @Inject
    public DeviceInformationServiceSettingViewModel(@NonNull SavedStateHandle savedStateHandle, @NonNull DeviceSettingRepository deviceSettingRepository) {
        super(deviceSettingRepository);
        mSavedStateHandle = savedStateHandle;

        mIsSystemIdSupported = savedStateHandle.getLiveData(KEY_IS_SYSTEM_ID_SUPPORTED);

        mSystemIdData = savedStateHandle.getLiveData(KEY_SYSTEM_ID_DATA);
        mModelNumberStringData = savedStateHandle.getLiveData(KEY_MODEL_NUMBER_STRING_DATA);
        mManufacturerNameStringData = savedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING_DATA);

        mManufacturerIdentifier = savedStateHandle.getLiveData(KEY_MANUFACTURER_IDENTIFIER);
        mOrganizationallyUniqueIdentifier = savedStateHandle.getLiveData(KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER);
        mModelNumberString = savedStateHandle.getLiveData(KEY_MODEL_NUMBER_STRING);
        mManufacturerNameString = savedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING);

        mSavedData = savedStateHandle.getLiveData(KEY_SAVED_DATA);
    }

    @Override
    public void observeSetup(@NonNull Intent intent, @NonNull Action onComplete, @NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
            if (mServiceData == null) {
                mServiceData = Utils.byteToParcelable(intent.getByteArrayExtra(DEVICE_INFORMATION_SERVICE.toString()), ServiceData.CREATOR);

                if (mServiceData == null) {
                    mServiceData = new ServiceData(DEVICE_INFORMATION_SERVICE
                            , BluetoothGattService.SERVICE_TYPE_PRIMARY
                            , new LinkedList<>());
                }
            }

            Optional<CharacteristicData> systemIdOptional = mServiceData.characteristicDataList
                    .stream()
                    .filter(characteristicData -> characteristicData.uuid.equals(SYSTEM_ID_CHARACTERISTIC))
                    .findAny();

            Optional<CharacteristicData> modelNumberStringOptional = mServiceData.characteristicDataList
                    .stream()
                    .filter(characteristicData -> characteristicData.uuid.equals(MODEL_NUMBER_STRING_CHARACTERISTIC))
                    .findAny();

            Optional<CharacteristicData> manufacturerNameStringOptional = mServiceData.characteristicDataList
                    .stream()
                    .filter(characteristicData -> characteristicData.uuid.equals(MANUFACTURER_NAME_STRING_CHARACTERISTIC))
                    .findAny();

            SystemId systemId;
            if (systemIdOptional.isPresent()) {
                CharacteristicData characteristicData = systemIdOptional.get();
                mSystemIdData.postValue(Utils.parcelableToByteArray(characteristicData));
                if (characteristicData.data == null) {
                    systemId = null;
                } else {
                    systemId = new SystemId(characteristicData.data);
                }
            } else {
                systemId = null;
            }

            if (mIsSystemIdSupported.getValue() == null) {
                mIsSystemIdSupported.postValue(systemIdOptional.isPresent());
            }

            if (mManufacturerIdentifier.getValue() == null) {
                if (systemId == null) {
                    mManufacturerIdentifier.postValue("");
                } else {
                    mManufacturerIdentifier.postValue(String.valueOf(systemId.getManufacturerIdentifier()));
                }
            }

            if (mOrganizationallyUniqueIdentifier.getValue() == null) {
                if (systemId == null) {
                    mOrganizationallyUniqueIdentifier.postValue("");
                } else {
                    mOrganizationallyUniqueIdentifier.postValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
                }
            }

            ModelNumberString modelNumberString;
            if (modelNumberStringOptional.isPresent()) {
                CharacteristicData characteristicData = modelNumberStringOptional.get();
                mModelNumberStringData.postValue(Utils.parcelableToByteArray(characteristicData));
                if (characteristicData.data == null) {
                    modelNumberString = null;
                } else {
                    modelNumberString = new ModelNumberString(characteristicData.data);
                }
            } else {
                modelNumberString = null;
            }

            if (mModelNumberString.getValue() == null) {
                if (modelNumberString == null) {
                    mModelNumberString.postValue("");
                } else {
                    mModelNumberString.postValue(modelNumberString.getModelNumber());
                }
            }

            ManufacturerNameString manufacturerNameString;
            if (manufacturerNameStringOptional.isPresent()) {
                CharacteristicData characteristicData = manufacturerNameStringOptional.get();
                mManufacturerNameStringData.postValue(Utils.parcelableToByteArray(characteristicData));
                if (characteristicData.data == null) {
                    manufacturerNameString = null;
                } else {
                    manufacturerNameString = new ManufacturerNameString(characteristicData.data);
                }
            } else {
                manufacturerNameString = null;
            }

            if (mManufacturerNameString.getValue() == null) {
                if (manufacturerNameString == null) {
                    mManufacturerNameString.postValue("");
                } else {
                    mManufacturerNameString.postValue(manufacturerNameString.getManufacturerName());
                }
            }

            emitter.onComplete();
        })
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(onComplete, onError));
    }

    @MainThread
    public void observeIsSystemIdSupported(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mIsSystemIdSupported).observe(owner, observer);
    }

    @MainThread
    public void observeHasSystemIdData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mSystemIdData).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasModelNumberStringData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mModelNumberStringData).observe(owner, new ExistObserver(observer));
    }

    @MainThread
    public void observeHasManufacturerNameStringData(@NonNull LifecycleOwner owner, @NonNull Observer<Boolean> observer) {
        Transformations.distinctUntilChanged(mManufacturerNameStringData).observe(owner, new ExistObserver(observer));
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
    public void observeModelNumberString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_MODEL_NUMBER_STRING)).observe(owner, observer);
    }

    @MainThread
    public void observeManufacturerNameString(@NonNull LifecycleOwner owner, @NonNull Observer<String> observer) {
        Transformations.distinctUntilChanged(mSavedStateHandle.<String>getLiveData(KEY_MANUFACTURER_NAME_STRING)).observe(owner, observer);
    }

    @MainThread
    public void observeSavedData(@NonNull LifecycleOwner owner, @NonNull Observer<Intent> observer) {
        mSavedData.observe(owner, observer);
    }

    @MainThread
    public void updateIsSystemIdSupported(boolean checked) {
        mIsSystemIdSupported.setValue(checked);
    }

    @Nullable
    @MainThread
    public byte[] getSystemIdData() {
        return mSystemIdData.getValue();
    }

    @MainThread
    public void setSystemIdData(@Nullable byte[] systemIdData) {
        mSystemIdData.setValue(systemIdData);
        MutableLiveData<String> manufacturerIdentifierLiveData = mSavedStateHandle.getLiveData(KEY_MANUFACTURER_IDENTIFIER);
        MutableLiveData<String> organizationallyUniqueIdentifierLiveData = mSavedStateHandle.getLiveData(KEY_ORGANIZATIONALLY_UNIQUE_IDENTIFIER);
        if (systemIdData == null) {
            manufacturerIdentifierLiveData.setValue(null);
            organizationallyUniqueIdentifierLiveData.setValue(null);
        } else {
            CharacteristicData characteristicData = Utils.byteToParcelable(systemIdData, CharacteristicData.CREATOR);
            if (characteristicData != null) {
                if (characteristicData.data == null) {
                    manufacturerIdentifierLiveData.setValue(null);
                    organizationallyUniqueIdentifierLiveData.setValue(null);
                } else {
                    SystemId systemId = new SystemId(characteristicData.data);
                    manufacturerIdentifierLiveData.setValue(String.valueOf(systemId.getManufacturerIdentifier()));
                    organizationallyUniqueIdentifierLiveData.setValue(String.valueOf(systemId.getOrganizationallyUniqueIdentifier()));
                }
            }
        }
    }

    @Nullable
    @MainThread
    public byte[] getModelNumberStringData() {
        return mModelNumberStringData.getValue();
    }

    @MainThread
    public void setModelNumberStringData(@Nullable byte[] modelNumberStringData) {
        mModelNumberStringData.setValue(modelNumberStringData);
        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_MODEL_NUMBER_STRING);
        if (modelNumberStringData == null) {
            liveData.setValue(null);
        } else {
            CharacteristicData characteristicData = Utils.byteToParcelable(modelNumberStringData, CharacteristicData.CREATOR);
            if (characteristicData != null) {
                if (characteristicData.data == null) {
                    liveData.setValue(null);
                } else {
                    liveData.setValue(new ModelNumberString(characteristicData.data).getModelNumber());
                }
            }
        }
    }

    @Nullable
    @MainThread
    public byte[] getManufacturerNameStringData() {
        return mManufacturerNameStringData.getValue();
    }

    @MainThread
    public void setManufacturerNameStringData(@Nullable byte[] manufacturerNameStringData) {
        mManufacturerNameStringData.setValue(manufacturerNameStringData);
        MutableLiveData<String> liveData = mSavedStateHandle.getLiveData(KEY_MANUFACTURER_NAME_STRING);
        if (manufacturerNameStringData == null) {
            liveData.setValue(null);
        } else {
            CharacteristicData characteristicData = Utils.byteToParcelable(manufacturerNameStringData, CharacteristicData.CREATOR);
            if (characteristicData != null) {
                if (characteristicData.data == null) {
                    liveData.setValue(null);
                } else {
                    liveData.setValue(new ManufacturerNameString(characteristicData.data).getManufacturerName());
                }
            }
        }
    }

    @Override
    public void save(@NonNull Consumer<? super Throwable> onError) {
        mDisposable.add(Completable.create(emitter -> {
                    if (mServiceData == null) {
                        emitter.onError(new RuntimeException("Already saved"));
                    } else {
                        mServiceData.characteristicDataList.clear();

                        if (Boolean.TRUE.equals(mIsSystemIdSupported.getValue())) {
                            byte[] systemId = mSystemIdData.getValue();
                            if (systemId != null) {
                                mServiceData.characteristicDataList.add(Utils.byteToParcelable(systemId, CharacteristicData.CREATOR));
                            }
                        }

                        byte[] modelNumberStringData = mModelNumberStringData.getValue();
                        if (modelNumberStringData != null) {
                            mServiceData.characteristicDataList.add(Utils.byteToParcelable(modelNumberStringData, CharacteristicData.CREATOR));

                        }

                        byte[] manufacturerNameStringData = mManufacturerNameStringData.getValue();
                        if (manufacturerNameStringData != null) {
                            mServiceData.characteristicDataList.add(Utils.byteToParcelable(manufacturerNameStringData, CharacteristicData.CREATOR));
                        }

                        if (mServiceData.characteristicDataList
                                .stream()
                                .filter(characteristicData -> !characteristicData.uuid.equals(SYSTEM_ID_CHARACTERISTIC)).count() == 2) {
                            Intent intent = new Intent();
                            intent.putExtra(DEVICE_INFORMATION_SERVICE.toString(), Utils.parcelableToByteArray(mServiceData));

                            mSavedData.postValue(intent);
                            mServiceData = null;
                            emitter.onComplete();
                        } else {
                            emitter.onError(new RuntimeException("No data"));
                        }
                    }
                }
        )
                .subscribeOn(Schedulers.io())
                .observeOn(AndroidSchedulers.mainThread())
                .subscribe(() -> {
                }, onError));
    }

}