package org.im97mori.ble.android.peripheral.ui.device.setting;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.graphics.drawable.Drawable;
import android.os.Build;
import android.text.TextUtils;
import android.view.View;
import android.widget.TextView;
import androidx.appcompat.widget.AppCompatImageView;
import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.HasDefaultViewModelProviderFactory;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.espresso.matcher.ViewMatchers;
import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputLayout;
import dagger.Module;
import dagger.Provides;
import dagger.hilt.InstallIn;
import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import dagger.hilt.android.testing.UninstallModules;
import dagger.hilt.components.SingletonComponent;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;
import junit.framework.TestCase;
import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.module.ViewModelFactoryFunctionModule;
import org.im97mori.ble.android.peripheral.test.FakeViewModelProviderFactoryFunction;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.FakeBloodPressureProfileViewModel;
import org.junit.After;
import org.junit.Before;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;

import static android.app.Activity.RESULT_OK;
import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.*;
import static junit.framework.TestCase.assertEquals;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;
import static org.im97mori.ble.android.peripheral.test.TestUtils.getCurrentMethodName;
import static org.junit.Assert.*;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
@UninstallModules(ViewModelFactoryFunctionModule.class)
public class DeviceSettingActivityTest {

    @Module
    @InstallIn(SingletonComponent.class)
    interface FakeViewModelFactoryFunctionModule {
        @Singleton
        @Provides
        public static Function<HasDefaultViewModelProviderFactory, ViewModelProvider.Factory> bindViewModelProviderFactoryFunction() {
            FakeViewModelProviderFactoryFunction fakeViewModelProviderFactoryFunction = new FakeViewModelProviderFactoryFunction();
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(DeviceSettingViewModel.class, FakeDeviceSettingViewModel.class);
            fakeViewModelProviderFactoryFunction.setFakeViewModelClass(BloodPressureProfileViewModel.class, FakeBloodPressureProfileViewModel.class);
            return fakeViewModelProviderFactoryFunction;
        }
    }

    @Rule(order = 1)
    public final HiltAndroidRule mHiltRule = new HiltAndroidRule(this);

    @Rule(order = 2)
    public final InstantTaskExecutorRule mInstantTaskRule = new InstantTaskExecutorRule();

    private ActivityScenario<DeviceSettingActivity> mScenario;

    private FakeDeviceSettingViewModel mFakeDeviceSettingViewModel;

    private FakeBloodPressureProfileViewModel mFakeBloodPressureProfileViewModel;

    @Inject
    @ApplicationContext
    Context mContext;

    @Before
    public void setUp() {
        RxJavaPlugins.setIoSchedulerHandler(scheduler -> Schedulers.trampoline());
        RxAndroidPlugins.setMainThreadSchedulerHandler(scheduler -> Schedulers.trampoline());

        mHiltRule.inject();
    }

    @After
    public void tearDown() {
        mScenario.close();
    }

    @Test
    public void test_title_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.setting))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_fragment_container_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });
        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        onView(withId(R.id.fragmentContainer)).check(matches(hasDescendant(withId(R.id.bloodPressureServiceCardView))));
    }

    @Test
    public void test_deviceSettingCardView_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.updateDeviceSettingName("");

        onView(withId(R.id.deviceSettingCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_deviceSettingCardView_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.updateDeviceSettingName("1");

        onView(withId(R.id.deviceSettingCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_deviceTypeImage_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                return ((AppCompatImageView) item).getDrawable() == null;
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + null);
            }
        }));
    }

    @Test
    public void test_deviceTypeImage_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Drawable drawable = item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + R.drawable.medical_ketsuatsukei_aneroid);
            }
        }));
    }

    @Test
    public void test_deviceTypeTitle_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.deviceTypeTitle)).check(matches(withText(R.string.device_type)));
    }

    @Test
    public void test_deviceType_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        onView(withId(R.id.deviceType)).check(matches(withText(R.string.blood_pressure_profile)));
    }

    @Test
    public void test_deviceSettingName_hint_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity
                -> assertEquals(mContext.getString(R.string.device_setting_name), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getHint()).toString()));
    }

    @Test
    public void test_deviceSettingName_error_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mScenario.onActivity(activity
                -> assertTrue(TextUtils.isEmpty(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getError())));
    }

    @Test
    public void test_deviceSettingName_error_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity
                -> assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getError()).toString()));
    }

    @Test
    public void test_updateDeviceSettingName_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        String original = "a";
        AtomicReference<String> deviceSettingName = new AtomicReference<>();
        mFakeDeviceSettingViewModel.mUpdateDeviceSettingNameConsumer = deviceSettingName::set;
        mScenario.onActivity(activity -> {
            TextView textView = activity.findViewById(R.id.deviceSettingNameEdit);
            textView.setText(original);
        });

        assertEquals(original, deviceSettingName.get());
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isNotEnabled()));
    }

    @Test
    public void test_menu_save_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        byte[] original = new byte[]{1};
        AtomicReference<byte[]> moduleDataString = new AtomicReference<>();
        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mUpdateMockDataStringConsumer = moduleDataString::set;

        onView(withId(R.id.save)).perform(click());

        mFakeBloodPressureProfileViewModel.mObserveSaveSubject.onNext(original);

        assertArrayEquals(original, moduleDataString.get());
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_activity_result_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launchActivityForResult(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        mFakeBloodPressureProfileViewModel.mObserveSaveSubject.onNext(new byte[]{1});

        mFakeDeviceSettingViewModel.updateDeviceSettingName("b");
        mFakeDeviceSettingViewModel.mFakeDeviceSettingRepository.mInsertDeviceSettingConsumer = deviceSetting -> {
        };

        onView(withId(R.id.save)).perform(click());

        mFakeBloodPressureProfileViewModel.mObserveSaveSubject.onNext(new byte[]{2});

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(RESULT_OK, activityResult.getResultCode());
    }

    @Test
    public void test_recreate_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext("");
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext("");
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));

        mScenario.recreate();

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext("");
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext("");
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_recreate_deviceSettingCardView_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.updateDeviceSettingName("");

        onView(withId(R.id.deviceSettingCardView)).check(matches(isNotChecked()));

        mScenario.recreate();

        onView(withId(R.id.deviceSettingCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_recreate_deviceSettingCardView_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.updateDeviceSettingName("1");

        onView(withId(R.id.deviceSettingCardView)).check(matches(isChecked()));

        mScenario.recreate();

        onView(withId(R.id.deviceSettingCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_recreate_deviceTypeImage_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                return ((AppCompatImageView) item).getDrawable() == null;
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + null);
            }
        }));

        mScenario.recreate();

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                return ((AppCompatImageView) item).getDrawable() == null;
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + null);
            }
        }));
    }

    @Test
    public void test_recreate_deviceTypeImage_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());
        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Drawable drawable = item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + R.drawable.medical_ketsuatsukei_aneroid);
            }
        }));

        mScenario.recreate();

        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Drawable drawable = item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid);
                assertNotNull(drawable);
                Bitmap bitmap = TestUtils.getBitmap(drawable);
                return targetBitmap.sameAs(bitmap);
            }

            @Override
            public void describeTo(Description description) {
                description.appendText("device_type:" + R.drawable.medical_ketsuatsukei_aneroid);
            }
        }));
    }

    @Test
    public void test_recreate_deviceType_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        onView(withId(R.id.deviceType)).check(matches(withText(R.string.blood_pressure_profile)));

        mScenario.recreate();

        onView(withId(R.id.deviceType)).check(matches(withText(R.string.blood_pressure_profile)));
    }

    @Test
    public void test_recreate_deviceSettingName_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.updateDeviceSettingName("1");
        onView(withId(R.id.deviceSettingNameEdit)).check(matches(withText("1")));

        mScenario.recreate();

        onView(withId(R.id.deviceSettingNameEdit)).check(matches(withText("1")));
    }

    @Test
    public void test_recreate_deviceSettingName_error_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = (FakeDeviceSettingViewModel) new ViewModelProvider(activity).get(DeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = (FakeBloodPressureProfileViewModel) new ViewModelProvider(activity).get(BloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupSubject.onNext(getCurrentMethodName());
        mFakeDeviceSettingViewModel.mFragmentReadySubject.onNext(getCurrentMethodName());

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getError()).toString()));

        mScenario.recreate();

        mScenario.onActivity(activity
                -> TestCase.assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getError()).toString()));
    }

}