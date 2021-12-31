package org.im97mori.ble.android.peripheral.ui.device.setting;

import static android.app.Activity.RESULT_OK;
import static androidx.test.espresso.Espresso.onView;
import static androidx.test.espresso.Espresso.pressBack;
import static androidx.test.espresso.action.ViewActions.click;
import static androidx.test.espresso.action.ViewActions.typeText;
import static androidx.test.espresso.assertion.ViewAssertions.matches;
import static androidx.test.espresso.matcher.ViewMatchers.hasDescendant;
import static androidx.test.espresso.matcher.ViewMatchers.isChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.isNotChecked;
import static androidx.test.espresso.matcher.ViewMatchers.isNotEnabled;
import static androidx.test.espresso.matcher.ViewMatchers.withEffectiveVisibility;
import static androidx.test.espresso.matcher.ViewMatchers.withId;
import static androidx.test.espresso.matcher.ViewMatchers.withText;
import static junit.framework.TestCase.assertEquals;
import static junit.framework.TestCase.assertTrue;
import static org.im97mori.ble.android.peripheral.Constants.DeviceTypes.DEVICE_TYPE_BLOOD_PRESSURE_PROFILE;
import static org.im97mori.ble.android.peripheral.Constants.IntentKey.KEY_DEVICE_TYPE;
import static org.mockito.Mockito.mockStatic;

import android.app.Activity;
import android.app.Instrumentation;
import android.content.Context;
import android.content.Intent;
import android.graphics.Bitmap;
import android.os.Build;
import android.text.TextUtils;
import android.view.View;

import androidx.appcompat.widget.AppCompatImageView;
import androidx.arch.core.executor.testing.InstantTaskExecutorRule;
import androidx.lifecycle.ViewModelProvider;
import androidx.test.core.app.ActivityScenario;
import androidx.test.espresso.matcher.ViewMatchers;

import com.google.android.material.appbar.MaterialToolbar;
import com.google.android.material.textfield.TextInputLayout;

import org.hamcrest.Description;
import org.hamcrest.TypeSafeMatcher;
import org.im97mori.ble.android.peripheral.R;
import org.im97mori.ble.android.peripheral.hilt.repository.FakeDeviceSettingRepository;
import org.im97mori.ble.android.peripheral.test.TestUtils;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.BloodPressureProfileViewModel;
import org.im97mori.ble.android.peripheral.ui.device.setting.fragment.blp.FakeBloodPressureProfileViewModel;
import org.im97mori.ble.android.peripheral.utils.MockitoViewModelProvider;
import org.junit.After;
import org.junit.AfterClass;
import org.junit.Before;
import org.junit.BeforeClass;
import org.junit.Rule;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.mockito.MockedStatic;
import org.robolectric.RobolectricTestRunner;
import org.robolectric.annotation.Config;

import java.util.Objects;
import java.util.concurrent.atomic.AtomicReference;

import javax.inject.Inject;

import dagger.hilt.android.qualifiers.ApplicationContext;
import dagger.hilt.android.testing.HiltAndroidRule;
import dagger.hilt.android.testing.HiltAndroidTest;
import dagger.hilt.android.testing.HiltTestApplication;
import io.reactivex.rxjava3.android.plugins.RxAndroidPlugins;
import io.reactivex.rxjava3.plugins.RxJavaPlugins;
import io.reactivex.rxjava3.schedulers.Schedulers;

@HiltAndroidTest
@RunWith(RobolectricTestRunner.class)
@Config(instrumentedPackages = {
        // required to access final members on androidx.loader.content.ModernAsyncTask
        "androidx.loader.content"}
        , application = HiltTestApplication.class
        , sdk = Build.VERSION_CODES.LOLLIPOP)
public class DeviceSettingActivityTest {

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

    @Inject
    FakeDeviceSettingRepository mFakeDeviceSettingRepository;

    private static MockedStatic<MockitoViewModelProvider> mockedStatic;

    @BeforeClass
    public static void setUpClass() {
        mockedStatic = mockStatic(MockitoViewModelProvider.class);
        mockedStatic.when(() -> MockitoViewModelProvider.getViewModelClass(DeviceSettingViewModel.class))
                .thenReturn(FakeDeviceSettingViewModel.class);
        mockedStatic.when(() -> MockitoViewModelProvider.getViewModelClass(BloodPressureProfileViewModel.class))
                .thenReturn(FakeBloodPressureProfileViewModel.class);
    }

    @AfterClass
    public static void tearDownClass() {
        mockedStatic.close();
    }

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
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        onView(withId(R.id.topAppBar)).check(matches(hasDescendant(withText(R.string.setting))));
    }

    @Test
    public void test_root_container_visibility_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.GONE)));
        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        onView(withId(R.id.rootContainer)).check(matches(withEffectiveVisibility(ViewMatchers.Visibility.VISIBLE)));
    }

    @Test
    public void test_fragment_container_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });
        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        onView(withId(R.id.fragmentContainer)).check(matches(hasDescendant(withId(R.id.bloodPressureServiceCardView))));
    }

    @Test
    public void test_deviceSettingCardView_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        mFakeDeviceSettingViewModel.updateDeviceSettingName("");

        onView(withId(R.id.deviceSettingCardView)).check(matches(isNotChecked()));
    }

    @Test
    public void test_deviceSettingCardView_00002() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        mFakeDeviceSettingViewModel.updateDeviceSettingName("1");

        onView(withId(R.id.deviceSettingCardView)).check(matches(isChecked()));
    }

    @Test
    public void test_deviceTypeImage_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
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
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        onView(withId(R.id.deviceTypeImage)).check(matches(new TypeSafeMatcher<View>() {
            @Override
            protected boolean matchesSafely(View item) {
                Bitmap targetBitmap = TestUtils.getBitmap(((AppCompatImageView) item).getDrawable());
                Bitmap bitmap = TestUtils.getBitmap(item.getContext().getDrawable(R.drawable.medical_ketsuatsukei_aneroid));
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
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        onView(withId(R.id.deviceTypeTitle)).check(matches(withText(R.string.device_type)));
    }

    @Test
    public void test_deviceType_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();

        onView(withId(R.id.deviceType)).check(matches(withText(R.string.blood_pressure_profile)));
    }

    @Test
    public void test_deviceSettingName_hint_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();

        mScenario.onActivity(activity
                -> assertEquals(mContext.getString(R.string.device_setting_name), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getHint()).toString()));
    }

    @Test
    public void test_deviceSettingName_error_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
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
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();

        mScenario.onActivity(activity
                -> assertEquals(mContext.getString(R.string.no_value), Objects.requireNonNull(((TextInputLayout) activity.findViewById(R.id.deviceSettingName)).getError()).toString()));
    }

    @Test
    public void test_updateDeviceSettingName_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();

        String original = "a";
        AtomicReference<String> deviceSettingName = new AtomicReference<>();
        mFakeDeviceSettingViewModel.mUpdateDeviceSettingNameConsumer = deviceSettingName::set;
        onView(withId(R.id.deviceSettingNameEdit)).perform(typeText(original));

        assertEquals(original, deviceSettingName.get());
    }

    @Test
    public void test_menu_save_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
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
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();

        mScenario.onActivity(activity -> ((MaterialToolbar) activity.findViewById(R.id.topAppBar)).showOverflowMenu());
        onView(withId(R.id.save)).check(matches(isEnabled()));
    }

    @Test
    public void test_menu_save_00003() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        String original = "a";
        AtomicReference<String> moduleDataString = new AtomicReference<>();
        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();
        mFakeDeviceSettingViewModel.mObserveSaveConsumer = moduleDataString::set;

        mFakeBloodPressureProfileViewModel.mGetModuleDataString = original;

        onView(withId(R.id.save)).perform(click());

        assertEquals(original, moduleDataString.get());
    }

    @Test
    public void test_backPressed_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        pressBack();
        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(Activity.RESULT_CANCELED, activityResult.getResultCode());
    }

    @Test
    public void test_activity_result_00001() {
        Intent intent = new Intent(mContext, DeviceSettingActivity.class);
        intent.putExtra(KEY_DEVICE_TYPE, DEVICE_TYPE_BLOOD_PRESSURE_PROFILE);
        mScenario = ActivityScenario.launch(intent);
        mScenario.onActivity(activity -> {
            mFakeDeviceSettingViewModel = new ViewModelProvider(activity).get(FakeDeviceSettingViewModel.class);
            mFakeBloodPressureProfileViewModel = new ViewModelProvider(activity).get(FakeBloodPressureProfileViewModel.class);
        });

        mFakeDeviceSettingViewModel.mObserveSetupProcessor.onComplete();
        mFakeDeviceSettingViewModel.mFragmentReadyProcessor.onComplete();

        mFakeBloodPressureProfileViewModel.mGetModuleDataString = "a";

        mFakeDeviceSettingViewModel.updateDeviceSettingName("b");
        mFakeDeviceSettingViewModel.mFakeDeviceSettingRepository.mInsertDeviceSettingConsumer = deviceSetting -> {
        };

        onView(withId(R.id.save)).perform(click());

        Instrumentation.ActivityResult activityResult = mScenario.getResult();
        assertEquals(RESULT_OK, activityResult.getResultCode());
    }

}